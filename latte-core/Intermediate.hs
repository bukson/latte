module Intermediate where

--

import AbsLatte
import ParLatte
import LexLatte
import System.IO
import System.Environment
import qualified Data.Map as Map
import Data.Maybe
import Data.Char(isNumber)
import Control.Monad.Except
import ErrM
import TypeChecker(check)
import TAC
import IntermediateEnv



 --those 2 functionts for testing only
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
        		mapM_ (\t -> putStrLn $ show t ) (translate topDefL)
        		putStrLn ""

translate :: [TopDef] -> [Tac]
translate topDefL = runIM $ gen topDefL

gen :: [TopDef] -> IM [Tac]
gen [] = do
	code <- getCode
	constants <- getConstants
	let constantsCode = map (\(Lab l, str) -> Constant l str) constants 
	let parcode = blockPartition (reverse code)
	return $ constantsCode ++ parcode
gen (d:ds) = do
	genTopDef d
	gen ds

genTopDef :: TopDef -> IM ()
genTopDef (FnDef t (Ident s) args (Block stmtL)) = do
	emit (FunLabel (Lab s))
	genLabel $ Lab "entry"
	renv <- IntermediateEnv.getEnv
	insertArgs args
	mapM_ genStmt stmtL
	case t of 
		Void -> do
			emit VReturn
			restoreEnv renv
		_ -> restoreEnv renv

genStmt :: Stmt -> IM()
genStmt Empty = return ()
genStmt (BStmt (Block stmtL)) = mapM_ genStmt stmtL
genStmt (Decl _ itemL) = do
	mapM_ genItem itemL where
		genItem (NoInit i) = insertIdent i
		genItem (Init i e) = do 
			insertIdent i
			genStmt (Ass (EVar i) e)
genStmt (Ass (EVar ident@(Ident i)) e) = do
	ins <- freshInstance ident
	t <- genExp e
	emit $ Ass1 ins t
genStmt (Ret e) = do
	t <- genExp e
	emit $ Return t
genStmt (VRet) = emit VReturn
-- TODO ładniej napisać to l1,l2,l3
genStmt (Cond e stmt) = do
	lTrue <- freshLabel
	lFalse <- freshLabel
	genCond e lTrue lFalse lTrue
	l0 <- getLastLabel
	--instances0 <- instances
	genLabel lTrue
	genStmt stmt
	emit (Jump lFalse)
	--instances1 <- instances
	genLabel lFalse
	--genFi l0 lTrue
genStmt (CondElse e stmt1 stmt2) = do
	lTrue <- freshLabel
	lFalse <- freshLabel
	lNext <- freshLabel
	genCond e lTrue lFalse lTrue
	genLabel lTrue
	genStmt stmt1
	emit (Jump lNext)
	--instancesTrue <- instances
	genLabel lFalse
	genStmt stmt2
	--instancesFalse <- instances
	emit (Jump lNext)
	genLabel lNext
	--genFi lTrue lFalse
-- While zwykły
--genStmt (While e stmt) = do
--	lEntry <- getLastLabel
--	lCond <- freshLabel
--	lBody <- freshLabel
--	lEnd <- freshLabel
--	emit (Jump lCond)
--	genLabel lCond
--	genFi lEntry lBody
--	genCond e lBody lEnd lBody
--	genLabel lBody
--	genStmt stmt
--	emit (Jump lCond)			
--	genLabel lEnd
genStmt (While e stmt) = do
	lEntry <- getLastLabel
	lBody <- freshLabel
	lCond <- freshLabel
	lEnd <- freshLabel
	emit (Jump lCond)
	genLabel lCond
	--genFi lEntry lBody
	genCond e lBody lEnd lEnd
	genLabel lBody
	genStmt stmt
	emit (Jump lCond)	
	emit (ChangeBlocs)
	genLabel lEnd
--genStmt(For arg e stmt) = do
genStmt(SExp e) = do
	genExp e
	return ()

genLabel :: Address -> IM()
genLabel l = do
	emit (Label l)
	setLabel l
	--newBloc

--genFi :: Address -> Address -> IM ()
--genFi l1 l2 = do
--	idents <- getIdents
--	mapM_ (\(Ident i) -> emit(Fi (Address i) [(l1, Address i), (l2, Address i)])) idents
--	return ()

genCond :: Expr -> Address -> Address -> Address -> IM ()
genCond (ERel e1 op e2) lThen lEsle lNext = do
	t1 <- genExp e1
	t2 <- genExp e2
	if lThen == lNext then
		emit(JmpCnd t1 (negRelOp $ translateRelOp op) t2 lEsle lThen)
	else
		emit (JmpCnd t1 (translateRelOp op) t2 lThen lEsle)
genCond (EAnd c1 c2) lTrue lFalse lNext = do
	lMid <- freshLabel
	genCond c1 lMid lFalse lMid
	emit (Label lMid)
	genCond c2 lTrue lFalse lNext
genCond (EOr c1 c2) lTrue lFalse lNext = do
	lMid <- freshLabel
	genCond c1 lTrue lMid lMid
	emit (Label lMid)
	genCond c2 lTrue lFalse lNext
genCond (AbsLatte.Not c) lTrue lFalse lNext = genCond c lFalse lTrue lNext
genCond e lTrue lFalse lNext = do
	t1 <- genExp e
	if lTrue == lNext then
		emit(JmpCnd t1 TAC.EQU (Const 0) lFalse lTrue)
	else
		emit (JmpCnd t1 (TAC.NE) (Const 0) lTrue lFalse)

genExp :: Expr -> IM Address
--genExp (ENewArr _ e) = do
--genExp (EField e i) = do
--genExp (EarrGet e1 e2) = do
genExp (EVar ident) = getInstance ident
genExp (ELitInt i) = return $ Const i
genExp (EString s) = do
	t <- insertConstant s
	return t
genExp (ELitTrue) = return $ Const 1
genExp (ELitFalse) = return $ Const 0
genExp (EApp ident@(Ident i) exprL) = do
	addrL <- mapM genExp exprL
	mapM_ (\t -> emit $ Param t) addrL
	t <- freshTemp 
	emit $ AssC t i (length addrL)
	return $ t
genExp (Neg e) = genBinOp (ELitInt 0) TAC.Minus e
genExp (AbsLatte.Not e) =  genBinOp (ELitInt 1) TAC.Minus e
genExp (EMul e1 r e2) = genBinOp e1 (translateMulOp r) e2
genExp (EAdd e1 r e2) = genBinOp e1 (translateAddOp r) e2
genExp (ERel e1 r e2) = genBinOp e1 (translateRelOp r) e2
genExp (EAnd e1 e2) = genBinOp e1 And e2
genExp (EOr e1 e2) = genBinOp e1 Or e2

genBinOp :: Expr -> Op -> Expr -> IM(Address)
genBinOp e1 r e2  = do
	t1 <- genExp e1
	t2 <- genExp e2
	t3 <- freshTemp
	emit (Ass3 t3 t1 r t2)
	return t3


