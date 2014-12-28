module TypeChecker where

-- 
-- ghc TypeChecker.hs -main-is TypeChecker.main

import AbsLatte
import ParLatte
import LexLatte
import System.IO
import System.Environment
import qualified Data.Map as Map
import Control.Monad.Except
import ErrM
--import ErrorT
import TypeCheckerEnv


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
    Ok p -> 
    	case TypeChecker.check p of
        	Left e -> putStrLn $ "Error " ++ e
        	Right _ -> putStrLn $ "Correct"

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



-- koniecznie rozdzielnie ze względu na parsing
--checkArgs :: [Arg] -> TCM ()
--checkArgs [] = return ()
--checkArgs ((Arg t ident):as) = do
--			case t of
--				StructT ident -> do
--					struct <- getVarT ident
--					case struct of
--						Nothing -> throwError $ errorT16 ident
--						Just _ -> checkArgs as
--				Dict k v -> checkDict (Dict k v) 
--				None -> throwError $ errorT20
--				_  -> checkArgs as
					

--checkArgSs :: [ArgS] -> TCM ()
--checkArgSs args = do
--			args' <- (mapM ( \(ArgS t i) -> return (Arg t i)) args)
--			checkArgs args'

--checkItemL :: Type -> [Item] -> TCM ()
--checkItemL _ [] = return ()
--checkItemL t (x:xs) = do
--	checkItem t x
--	checkItemL t xs

--checkItem :: Type -> Item -> TCM ()
--checkItem _ (NoInit _ ) = return ()
--checkItem t (Init li@(Ident i) e) = do
--	et <- getExprT e
--	if t == et 
--		then
--			return ()
--		else
--			throwError $ errorT1 i t et


--checkReturn :: [Stmt] -> Type -> TCM (Bool)
--checkReturn _  None = return True
--checkReturn [] t = return False
--checkReturn (s:ss) t = case s of
--	BStmt (Blck bs) -> do
--				bret <- checkReturn bs t
--				ssret <- checkReturn ss t
--				if (bret || ssret) 
--					then return True
--					else return False
--	VRet 	-> 	return True
--	Ret e 	-> 	return True
--	CondElse _ s1 s2  -> do
--				r1 <- checkReturn [s1] t
--				r2 <- checkReturn [s2] t
--				if r1 && r2 
--					then return True
--					else checkReturn ss t
--	_ ->		checkReturn ss t 


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
		Just _ -> throwError "redeclaration of function"

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
	when (not ifRet) (throwError $ "possible not return statement in function")
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
checkStmt (Decl t []) = throwError $ "errorT9" 
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
									throwError "error" 
checkStmt (Ass e1 e2) = do
	case e1 of
		EVar _ -> chck e1 e2
		EArrGet e1 e2 -> chck e1 e2
		_ -> throwError $ "bad l-value"
	where 
		chck :: Expr -> Expr -> TCM ()
		chck e1 e2 = do
			e1t <- getExprT e1
			e2t <- getExprT e2
			if (e2t /= e1t)
				then throwError $ "bad type assignment"
				else return ()		
checkStmt (BStmt (Block stmts)) = checkStmtL stmts
checkStmt (Incr ident@(Ident i)) = do
	it <- getVarType ident
	case it of
		Just Int -> return ()
		Just t -> throwError $ "errorT7 (EVar ident) Int t"
		Nothing -> throwError $ "errorT2 i"
checkStmt (Decr ident@(Ident i)) = do
	it <- getVarType ident
	case it of
		Just Int -> return ()
		Just t -> throwError $ "errorT7 (EVar ident) Int t"
		Nothing -> throwError $ "errorT2 i "
checkStmt (Ret e) = do
	maybe_ft <- getCurrFunRetType
	et <- getExprT e
	case maybe_ft of
		Just ft -> do
			if (et == ft)
				then return ()
				else throwError $ "bad return type"
		Nothing -> throwError $ "unknown error"
checkStmt (VRet) = do
		maybe_ft <- getCurrFunRetType
		case maybe_ft of
			Just Void -> return ()
			Just _ -> throwError $ "fucntion type is not void"
			Nothing -> throwError $ "unknown error"
checkStmt (Cond expr stmt) = do
	exprT <- getExprT expr
	if (exprT == Bool)
		then checkStmt stmt
		else throwError $ "errorT7 e Bool et"
checkStmt (CondElse expr stmt1 stmt2) = do
	exprT <- getExprT expr
	if (exprT == Bool)
		then do
			checkStmt stmt1
			checkStmt stmt2
		else throwError $ "errorT7 e Bool et"
checkStmt (While e stmt) = do
	et <- getExprT e
	if (et == Bool)
		then checkStmt stmt
		else throwError $ "errorT7 e Bool et"
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
					throwError "bad types"
			_ -> throwError "array expected"
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
		_ -> throwError "Expected int"
getExprT (ECast t) = return t
getExprT (EArrGet e1 e2) = do
	t1 <- getExprT e1
	t2 <- getExprT e2
	case t1 of
		(Array t) -> case t2 of
						Int -> return t
						_ -> throwError "Int expected" 
		_ -> throwError "Array expected"
getExprT (EVar li@(Ident i)) = do
	t <- getVarType li
	case t of 
		Nothing -> throwError "$ errorT2 i"
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
							else throwError $ "errorT3 argTL e2t"
		_ -> throwError $ "errorT4 e1 "
getExprT (Neg e) = do
	t <- getExprT e
	if t == Int
		then return Int
		else throwError $ "errorT7 e Int t"
getExprT (Not e) = do
	t <- getExprT e
	if t == Bool
		then return Bool
		else throwError $ "errorT7 e Bool t"
getExprT (EMul e1 _ e2) = do
	t1 <- getExprT e1
	t2 <- getExprT e2
	case (t1,t2) of
		(Int, Int) -> return Int
		(Int, _) -> throwError $ "errorT7 e2 Int t2"
		(_, _) -> throwError $ "errorT7 e1 Int t1 "
getExprT (EAdd e1 _ e2) = do
	t1 <- getExprT e1
	t2 <- getExprT e2
	case (t1,t2) of
		(Int, Int) -> return Int
		(Str, Str) -> return Str
		(Int, _) -> throwError $ "errorT7 e2 Int t2"
		(Str, _) -> throwError $ "errorT7 e2 Str t2 "
		(_, _) -> throwError $ "errorT8 e1 t1"
getExprT (ERel e1 r e2) = do
	t1 <- getExprT e1
	t2 <- getExprT e2 
	case r of
		EQU -> if t1 == t2
						then return Bool
						else throwError $ "errorT7 e2 t2 t1" 
		NE -> if t1 == t2
						then return Bool
						else throwError $ "errorT7 e2 t2 t1"
		_ -> case (t1,t2) of
			(Int, Int) -> return Bool
			(Int, _) -> throwError $ "errorT7 e2 Int t2"
			(_, _) -> throwError $ "errorT7 e1 Int t1"
getExprT (EAnd e1 e2) = do
	t1 <- getExprT e1
	t2 <- getExprT e2
	case (t1,t2) of
		(Bool, Bool) -> return Bool
		(Bool, _) -> throwError "errorT7 e2 Bool t2"
		(_, _) -> throwError $ "errorT7 e1 Bool t1" 
getExprT (EOr e1 e2) = do
	t1 <- getExprT e1
	t2 <- getExprT e2
	case (t1,t2) of
		(Bool, Bool) -> return Bool
		(Bool, _) -> throwError $ "errorT7 e2 Bool t2"
		(_, _) -> throwError $ "errorT7 e1 Bool t1"

