module CopyO where

--
import Control.Monad.State
import TAC
import qualified Data.Map as Map

type CopyM a = State Env a

data Env = Env {
	idents :: Map.Map Address Address,
	lastTmp:: Tac
}

runCopyM :: CopyM a -> a
runCopyM m = evalState m (Env Map.empty Empty)

insertCopy :: Address -> Address -> CopyM ()
insertCopy a1 a2 = modify $ \s -> s {idents = Map.insert a1 a2 (idents s)} 

propagate :: Address -> CopyM Address
propagate a = do
	maybe_copy <- gets $ \s -> Map.lookup a (idents s) 
	case maybe_copy of
		Just copy -> return copy
		Nothing -> return a 

copyO :: [Tac] -> [Tac]
copyO insL = let 
	insL' = runCopyM (mapM optTmp insL)
	in runCopyM (mapM optCopies insL')

optCopies :: Tac -> CopyM Tac
optCopies (Blck insL) = do
	insL' <- mapM optCopies insL
	return $ Blck insL'
optCopies (Ass1 a1 a2) = do
	a2' <- propagate a2
	insertCopy a1 a2'
	return $ Ass1 a1 a2
optCopies (Ass3 typ a1 a2 op a3) = do
	a2' <- propagate a2
	a3' <- propagate a3
	return $ Ass3 typ a1 a2' op a3'
optCopies (Fi a t ((l1,v1):(l2,v2):[])) = do
	v1' <- propagate v1
	v2' <- propagate v2
	return $ Fi a t ((l1,v1'):(l2,v2'):[])
optCopies (FunLabel typ l args) = do
	modify $ \s -> s{idents = Map.empty}
	return $ FunLabel typ l args
optCopies (AssC a ident typ argTypes addrL) = do
	addrL' <- mapM propagate addrL
	if typ == "void" then
		return $ Call ident argTypes addrL'
	else
		return $ AssC a ident typ argTypes addrL'
optCopies (Return typ a) = do
	a' <- propagate a
	return $ Return typ a'
optCopies t = return t


optTmp :: Tac -> CopyM Tac
optTmp (Blck insL) = do
	insL' <- mapM optTmp insL
	return $ Blck insL'
optTmp (Ass1 a1 (Address "_t" n)) = do
	Ass3 typ _ a2 op a3  <- gets lastTmp 
	return $ Ass3 typ a1 a2 op a3
optTmp ins@(Ass3 t (Address "_t" n) a2 op a3) = do
	modify $ \s -> s{lastTmp = ins}
	return ins
optTmp ins = return ins


