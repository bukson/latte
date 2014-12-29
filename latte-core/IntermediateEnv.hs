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

freshTemp :: IM Var
freshTemp = do
		fresh <- gets fTemp
		modify (\s -> Env (idents s) (fresh + 1) (fLabel s) (code s))
		return $ Var ("_t" ++ (show (fresh + 1)))

freshLabel :: IM Label
freshLabel = do
		fresh <- gets fLabel
		modify (\s -> Env (idents s) (fTemp s) (fresh + 1) (code s))
		return $ Label ("L" ++ (show (fresh + 1)))

insertIdent :: Ident -> IM()
insertIdent i = modify (\s -> Env (Map.insert i 0 (idents s)) (fTemp s) (fLabel s) (code s))

getInstance :: Ident -> IM Var
getInstance i@(Ident s) = do
	maybe_n <- gets (\s -> Map.lookup i (idents s))
	return $ Var (s ++ (show $ fromJust maybe_n))

freshInstance :: Ident -> IM Var
freshInstance i@(Ident s) = do
	maybe_n <- gets (\s -> Map.lookup i (idents s))
	modify (\s -> Env (Map.insert i ((fromJust maybe_n) + 1) (idents s)) (fTemp s) (fLabel s) (code s))
	return $ Var (s ++ (show $ (fromJust maybe_n) + 1))

getCode :: IM [Tac]
getCode = gets code

emit :: Tac -> IM ()
emit instruction = do
	c <- getCode
	modify(\s -> Env (idents s) (fTemp s) (fLabel s) (instruction:c))

insertArgs :: [Arg] -> IM()
insertArgs args = mapM_ (\a@(Arg _ i) -> insertIdent i) args

getEnv :: IM(Env)
getEnv = get

-- restoreEnv saves changes in code
restoreEnv :: Env -> IM()
restoreEnv s' = do
	c <- getCode 
	put s'
	modify(\s -> Env (idents s) (fTemp s) (fLabel s) c)


-- rzygam haskelem czasami, ale i tak jest najlepszy
translateRelOp :: RelOp -> Op
translateRelOp AbsLatte.LTH = TAC.LTH
translateRelOp AbsLatte.LE  = TAC.LE
translateRelOp AbsLatte.GTH = TAC.GTH
translateRelOp AbsLatte.GE  = TAC.GE
translateRelOp AbsLatte.EQU = TAC.EQU
translateRelOp AbsLatte.NE  = TAC.NE

translateAddOp :: AddOp -> Op
translateAddOp AbsLatte.Plus = TAC.Plus
translateAddOp AbsLatte.Minus = TAC.Minus

translateMulOp :: MulOp -> Op
translateMulOp AbsLatte.Times = TAC.Times
translateMulOp AbsLatte.Div = TAC.Div
translateMulOp AbsLatte.Mod = TAC.Mod