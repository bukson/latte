module CopyO where

--
import Control.Monad.State
import TAC
import qualified Data.Map as Map

type CopyM a = State Env a

data Env = Env {
	idents :: Map.Map Address Address
}

runCopyM :: CopyM a -> a
runCopyM m = evalState m (Env Map.empty)

insertCopy :: Address -> Address -> CopyM ()
insertCopy a1 a2 = modify $ \s -> s {idents = Map.insert a1 a2 (idents s)} 

propagate :: Address -> CopyM Address
propagate a = do
	maybe_copy <- gets $ \s -> Map.lookup a (idents s)
	case maybe_copy of
		Just copy -> return copy
		Nothing -> return a 

copyO :: [Tac] -> [Tac]
copyO insL = runCopyM (mapM opt insL) 

opt :: Tac -> CopyM Tac
opt (Blck insL) = do
	insL' <- mapM opt insL
	return $ Blck insL'
opt (Ass1 a1 a2) = do
	a1' <- propagate a1
	a2' <- propagate a2
	insertCopy a1' a2'
	return $ Ass1 a1' a2'
opt (Ass2 a1 op a2) = do
	a1' <- propagate a1
	a2' <- propagate a2
	return $ Ass2 a1' op a2'
opt (Ass3 a1 a2 op a3) = do
	a2' <- propagate a2
	a3' <- propagate a3
	return $ Ass3 a1 a2' op a3'
opt t = return t

