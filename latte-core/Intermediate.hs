module Intermediate where

--

import AbsLatte
import ParLatte
import LexLatte
import System.IO
import System.Environment
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Except
import ErrM
import TypeChecker(check)
import TAC
import IntermediateEnv



-- those 2 functionts for testing only
main :: IO()
main = do
  name <- getProgName
  args <- getArgs
  case args of 
    [] -> getContents >>= proceed
    [n] -> readFile n >>= proceed
    otherwise -> putStrLn $ "Unkown error."

proceed :: String -> IO()
proceed s = 
  case pProgram $ myLexer s of
    Bad a -> 
    	putStrLn a
    Ok p@(Program topDefL) -> 
    	case check p of
        	Left e -> putStrLn $ "Error " ++ e
        	Right _ -> do
        		mapM_ (\t -> putStrLn $ show t ) (runIM $ gen topDefL)
        		putStrLn ""


gen :: [TopDef] -> IM [Tac]
gen [] = do
	code <- getCode
	return $ reverse code
gen (d:ds) = do
	genTopDef d
	gen ds

genTopDef :: TopDef -> IM ()
genTopDef (FnDef _ (Ident s) args (Block stmtL)) = do
	emit (Lab (Label s))
	renv <- IntermediateEnv.getEnv
	insertArgs args
	mapM_ genStmt stmtL
	restoreEnv renv

genStmt :: Stmt -> IM()
genStmt Empty = return ()
genStmt (BStmt (Block stmtL)) = mapM_ genStmt stmtL
genStmt (Ass (EVar i) e) = do
	var <- genExp e
	freshI <- freshInstance i
	emit $ Ass1 freshI var

--genStmt (EarrGet e1 e2) = do
genStmt(SExp e) = do
	genExp e
	return ()

genExp :: Expr -> IM(Var)
--genExp (ECast t) = return 
genExp (EVar i) = getInstance i
genExp (ELitInt i) = return $ ConstI i
genExp (EString s) = return $ ConstS s
genExp (ELitTrue) = return $ ConstB True
genExp (ELitFalse) = return $ ConstB False
genExp (EMul e1 r e2) = genBinOp e1 (translateMulOp r) e2
genExp (EAdd e1 r e2) = genBinOp e1 (translateAddOp r) e2
genExp (ERel e1 r e2) = genBinOp e1 (translateRelOp r) e2
genExp (EAnd e1 e2) = genBinOp e1 And e2
genExp (EOr e1 e2) = genBinOp e1 Or e2

genBinOp :: Expr -> Op -> Expr -> IM(Var)
genBinOp e1 r e2  = do
	t1 <- freshTemp
	t2 <- genExp e1
	t3 <- genExp e2
	emit (Ass3 t1 t2 r t3)
	return t1








