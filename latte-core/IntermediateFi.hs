module IntermediateFi where

--
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe(fromJust)
import TAC

type PM a = State Env a

data Env = Env {
	dump :: Map.Map String Address
}

runPM :: PM a ->  a
runPM m  = evalState m (Env Map.empty)

properFi :: [Tac] -> [Tac]
properFi insL = runPM $ mapM proper insL

proper :: Tac -> PM Tac
proper (Blck insL) = do
	insL' <- mapM proper insL
	return $ Blck insL'
proper (Dump m) = do
	modify $ \s -> s{dump = m}
	return Empty
proper (TmpFi a typ s ((l1,v1):(l2, v_uknown):[])) = do
	dump_map <- gets dump
	let Just v2 = Map.lookup s dump_map
	if v2 == a then
		return $ Ass1 a v1
	else
		return $ (Fi a typ [(l1,v1),(l2, v2)])
proper t = return t


