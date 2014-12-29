module TypeChecker where

-- 
-- ghc TypeChecker.hs -main-is TypeChecker.main

import AbsLatte
import ParLatte
import LexLatte
import System.IO
import System.Environment
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Except
import ErrM
import TypeCheckerError
import TypeCheckerEnv


--main :: IO()
--main = do
--  name <- getProgName
--  args <- getArgs
--  case args of 
--    [] -> getContents >>= proceed
--    [n] -> readFile n >>= proceed
--    otherwise -> putStrLn $ "Unkown error."

--proceed :: String -> IO()
--proceed s = 
--  case pProgram $ myLexer s of
--    Bad a -> 
--    	putStrLn a
--    Ok p -> 
--    	case TypeChecker.check p of
--        	Left e -> putStrLn $ "Error " ++ e
--        	Right _ -> putStrLn $ "Correct"

-- Insert into env built in functions
initBuiltIn :: TCM ()
initBuiltIn = do
	putFun (Ident "printInt") (Fun Void [Int])
	putFun (Ident "printString") (Fun Void [Str])
	putFun (Ident "error") (Fun Void [])
	putFun (Ident "readInt") (Fun Int [])
	putFun (Ident "readString") (Fun Str [])

check :: Program -> Either String ()
check p = runTCM (checkProgram p)

checkProgram :: Program -> TCM ()
checkProgram (Program defs) = do
	initBuiltIn
	addDeclarations defs
	checkTopDefL defs

------------------------------------------------------------------------------------------------
-- TOP DEFINITIONS -----------------------------------------------------------------------------
------------------------------------------------------------------------------------------------


addDeclarations :: [TopDef] -> TCM ()
addDeclarations [] = return ()
addDeclarations (d:ds) = do
	addDeclaration d
	addDeclarations ds

addDeclaration :: TopDef -> TCM ()
addDeclaration (FnDef retT ident args (Block stmts)) = do
	argsTypes <- getArgsTypes args
	redeclaration <- getFunType ident
	case redeclaration of
		Nothing -> putFun ident (Fun retT argsTypes)
		Just _ -> throw_error $ (redeclarationE ident)

checkTopDefL :: [TopDef] -> TCM ()
checkTopDefL [] = return ()
checkTopDefL (d:ds) = do
	checkTopDef d
	checkTopDefL ds

checkReturn :: [Stmt] -> Type -> TCM(Bool)
checkReturn _ Void = return True
checkReturn [] t = return False
checkReturn(stmt:stmts) t = case stmt of
	BStmt (Block bs) -> do
			bRet <- checkReturn bs t
			stmtsRet <- checkReturn stmts t
			return $ bRet || stmtsRet
	VRet -> return True
	Ret _ -> return True
	Cond ELitTrue s -> do
		r <- checkReturn [s] t
		if r then
			return True
		else
			checkReturn stmts t 
	CondElse ELitTrue s1 _ -> do
		r1 <- checkReturn [s1] t
		if r1 then
			return True
		else
			checkReturn stmts t 
	CondElse ELitFalse _ s2 -> do
		r2 <- checkReturn [s2] t
		if r2 then
			return True
		else
			checkReturn stmts t 
	CondElse _ s1 s2 -> do
			r1 <- checkReturn [s1] t
			r2 <- checkReturn [s2] t
			if r1 && r2 then
				return True
			else
				checkReturn stmts t
	_ -> checkReturn stmts t

checkTopDef :: TopDef -> TCM()
checkTopDef (FnDef t ident args (Block stmts)) = do 
	renv <- TypeCheckerEnv.getEnv --getEnv
	insertArgs args
	setCurrFun ident
	checkStmtL stmts
	ifRet <- checkReturn stmts t
	when (not ifRet) (throw_error noReturnE)
	restoreEnv renv
	return ()

------------------------------------------------------------------------------------------------
-- STATEMENTS ----------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

checkStmtL :: [Stmt] -> TCM ()
checkStmtL [] = return ()
checkStmtL (s:ss) = do
	checkStmt s
	checkStmtL ss

checkStmt :: Stmt -> TCM ()
checkStmt Empty = return ()
checkStmt (Decl t il) = do
	checkDec il 
	where
		checkDec :: [Item] -> TCM ()
		checkDec [] = return ()
		checkDec (it:itls) = do
			--checkItem t it
			case it of
				NoInit ident -> do
								(putVar ident t) 
								checkDec itls
				Init ident expr -> do
								exprT <- getExprT expr
								if exprT == t then do
									(putVar ident t)
									checkDec itls
								else
									throw_error $ badTypeE t exprT expr 
checkStmt (Ass e1 e2) = do
	case e1 of
		EVar _ -> do
			e1t <- getExprT e1
			e2t <- getExprT e2
			if (e2t /= e1t)
				then throw_error $ badTypeE e1t e2t e2 
				else return ()		
		EArrGet e1 e2 -> do
			e1t <- getExprT e1
			e2t <- getExprT e2
			case e1t of
				Array at -> do
					if (at /= e2t)
					then throw_error $ badTypeE at e2t e2 
					else return ()		
				_ -> throw_error $ unknownE
		_ -> throw_error $ lValueE e1 
checkStmt (BStmt (Block stmts)) = checkStmtL stmts
checkStmt (Incr ident@(Ident i)) = do
	it <- getVarType ident
	case it of
		Just Int -> return ()
		Just t -> throw_error $ integerE (EVar ident) 
		Nothing -> throw_error $ identE ident 
checkStmt (Decr ident@(Ident i)) = do
	it <- getVarType ident
	case it of
		Just Int -> return ()
		Just t -> throw_error $ integerE (EVar ident) 
		Nothing -> throw_error $ identE ident 
checkStmt (Ret e) = do
	ft <- getCurrFunRetType
	et <- getExprT e
	if (et == (fromJust ft))
		then return ()
		else throw_error $ badTypeE (fromJust ft) et e 
checkStmt (VRet) = do
		maybe_ft <- getCurrFunRetType
		case maybe_ft of
			Just Void -> return () 
			Just _ -> throw_error $ voidE 
			Nothing -> throw_error $ unknownE 
checkStmt (Cond expr stmt) = do
	exprT <- getExprT expr
	if (exprT == Bool)
		then checkStmt stmt
		else throw_error $ badTypeE Bool exprT expr 
checkStmt (CondElse expr stmt1 stmt2) = do
	exprT <- getExprT expr
	if (exprT == Bool)
		then do
			checkStmt stmt1
			checkStmt stmt2
		else throw_error $ badTypeE Bool exprT expr 
checkStmt (While expr stmt) = do
	exprT <- getExprT expr
	if (exprT == Bool)
		then checkStmt stmt
		else throw_error $ badTypeE Bool exprT expr 
checkStmt (For (Arg argT argIdent) expr stmt ) = do
		renv <- TypeCheckerEnv.getEnv
		identT <- getExprT expr
		case identT of
			Array arrayT -> 
				if argT == arrayT then do
					putVar argIdent argT
					checkStmt stmt
					restoreEnv renv
					return ()
				else
					throw_error $ badTypeE arrayT argT expr 
			_ -> throw_error $ arrayE expr 
checkStmt (SExp e) = do
	t <- getExprT e
	return ()

------------------------------------------------------------------------------------------------
-- EXPRESSIONS ---------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

getExprT :: Expr -> TCM Type 
getExprT (ENewArr t expr) = do
	et <- getExprT expr
	case et of
		Int -> return $ Array t
		_ -> throw_error $ integerE expr
getExprT e@(EField expr ident) = do
	exprT <- getExprT expr
	case (exprT, ident) of
	 	(Array _, Ident "length") -> return Int
	 	(_, _) -> throw_error $ dotE e
getExprT (ECast t) = return t
getExprT (EArrGet e1 e2) = do
	t1 <- getExprT e1
	t2 <- getExprT e2
	case t1 of
		(Array t) -> case t2 of
						Int -> return t
						_ -> throw_error $ integerE e2
		_ -> throw_error $ arrayE e1
getExprT (EVar li@(Ident i)) = do
	t <- getVarType li
	case t of 
		Nothing -> throw_error $ identE li
		Just t -> return t
getExprT ELitTrue = return Bool
getExprT ELitFalse = return Bool
getExprT (EString _) = return Str
getExprT (ELitInt _) = return Int
getExprT (EApp ident es) = do
	identT <- getFunType ident
	est  <- mapM getExprT es
	case identT of
		Just (Fun retT argTL) -> if argTL == est 
							then return retT 
							else throw_error $ appE argTL est
		_ -> throw_error $ callE (EVar ident)
getExprT (Neg e) = do
	t <- getExprT e
	if t == Int
		then return Int
		else throw_error $ integerE e
getExprT (Not e) = do
	t <- getExprT e
	if t == Bool
		then return Bool
		else throw_error $ badTypeE Bool t e
getExprT (EMul e1 _ e2) = do
	t1 <- getExprT e1
	t2 <- getExprT e2
	case (t1,t2) of
		(Int, Int) -> return Int
		(Int, _) -> throw_error $ integerE e2
		(_, _) -> throw_error $ integerE e1
getExprT (EAdd e1 _ e2) = do
	t1 <- getExprT e1
	t2 <- getExprT e2
	case (t1,t2) of
		(Int, Int) -> return Int
		(Str, Str) -> return Str
		(Int, _) -> throw_error $ badTypeE Int t2 e2
		(Str, _) -> throw_error $ badTypeE Str t2 e2
		(_, _) -> throw_error $ addE e1 e2
getExprT (ERel e1 r e2) = do
	t1 <- getExprT e1
	t2 <- getExprT e2 
	case r of
		EQU -> if t1 == t2
						then return Bool
						else throw_error $ badTypeE t1 t2 e2
		NE -> if t1 == t2
						then return Bool
						else throw_error $ badTypeE t1 t2 e2
		_ -> case (t1,t2) of
			(Int, Int) -> return Bool
			(Int, _) -> throw_error $ badTypeE Int t2 e2
			(_, _) -> throw_error $ badTypeE Int t1 e1
getExprT (EAnd e1 e2) = do
	t1 <- getExprT e1
	t2 <- getExprT e2
	case (t1,t2) of
		(Bool, Bool) -> return Bool
		(Bool, _) -> throw_error $ badTypeE Bool t2 e2
		(_, _) -> throw_error $ badTypeE Bool t1 e1
getExprT (EOr e1 e2) = do
	t1 <- getExprT e1
	t2 <- getExprT e2
	case (t1,t2) of
		(Bool, Bool) -> return Bool
		(Bool, _) -> throw_error $ badTypeE Bool t2 e2
		(_, _) -> throw_error $ badTypeE Bool t1 e1

