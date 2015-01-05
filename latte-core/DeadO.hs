module DeadO where

--
import Control.Monad.State
import TAC
import qualified Data.Set as Set

type DeadM a = State Env a

data Env = Env {
	used :: Set.Set Address
}

runDeadM :: DeadM a -> a
runDeadM m = evalState m (Env Set.empty)

deadO :: [Tac] -> [Tac]
deadO insL = let 
	insL' = runDeadM (mapM optDead (reverse insL))
	in reverse insL'

isDead :: Address -> DeadM Bool
isDead a = do
	is_used <- gets $ \s -> Set.member a (used s)
	return $ not is_used

addUsed :: Address -> DeadM ()
addUsed a = do
	modify $ \s -> s {used = Set.insert a (used s)}
	return ()

optDead :: Tac -> DeadM Tac
optDead (Blck insL) = do
	insL' <- mapM optDead (reverse insL)
	return $ Blck (reverse insL')
optDead ins @(AssC a1 ident typ argsT args) = do
	mapM_ addUsed args
	return ins
optDead ins@(Ass1 a1 a2) = do
	a1_dead <- isDead a1
	if a1_dead then
		return Empty
	else do
		addUsed a2
		return ins
optDead ins@(Ass3 t a1 a2 op a3) = do
	a1_dead <- isDead a1
	if a1_dead then
		return Empty
	else do
		addUsed a2
		addUsed a3
		return ins
optDead ins@(Fi a t ((l1,v1):(l2,v2):[])) = do
	--a_dead <- isDead a
	--if a_dead then
		--return Empty
	--else do
		addUsed v1
		addUsed v2
		return ins
optDead ins@(FunLabel typ l args) = do
	modify $ \s -> s {used = Set.empty}
	return ins
optDead ins@(Return typ a) = do
	addUsed a
	return ins
optDead ins@(JmpCnd a l1 l2) = do
	addUsed a
	return ins
optDead ins = return ins
