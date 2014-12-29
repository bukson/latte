module IntermediateEnv where

--
import AbsLatte
import LexLatte
import ParLatte
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe
import ErrM
import TAC

type IM a = State Env a

data Env = Env {
	idents :: Map.Map Ident Integer,
	fTemp :: Integer,
	fLabel :: Integer,
	code :: [Tac]
}

emptyEnv = Env Map.empty 0 0 []

runIM :: IM a -> a
runIM m = evalState m emptyEnv

freshTemp :: IM String
freshTemp = do
		fresh <- gets fTemp
		modify (\s -> Env (idents s) (fresh + 1) (fLabel s) (code s))
		return $ "_t" ++ (show fresh)

freshLabel :: IM String
freshLabel = do
		fresh <- gets fLabel
		modify (\s -> Env (idents s) (fTemp s) (fresh + 1) (code s))
		return $ "L" ++ (show fresh)

insertIdent :: Ident -> IM ()
insertIdent i = modify (\s -> Env (Map.insert i 0 (idents s)) (fTemp s) (fLabel s) (code s))

getCode :: IM [Tac]
getCode = gets code

emit :: Tac -> IM ()
emit instruction = do
	c <- getCode
	modify(\s -> Env (idents s) (fTemp s) (fLabel s) (instruction:c))

insertArgs :: [Arg] -> IM ()
insertArgs args = mapM_ (\a@(Arg _ i) -> insertIdent i) args

getEnv :: IM(Env)
getEnv = get

-- restoreEnv saves changes in code
restoreEnv :: Env -> IM()
restoreEnv s' = do
	c <- getCode 
	put s'
	modify(\s -> Env (idents s) (fTemp s) (fLabel s) c)