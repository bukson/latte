module TypeCheckerEnv where

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

--import ErrorT

type TCM a = StateT Env (Either String) a 

data Env = Env {
	idents :: Map.Map Ident Type,
	functions :: Map.Map Ident FunType,
	currFun :: Maybe Ident
}

emptyEnv = Env Map.empty Map.empty Nothing

runTCM :: TCM a -> Either String a
runTCM m = evalStateT m emptyEnv 

getVarType :: Ident -> TCM (Maybe Type)
getVarType li = gets (\s -> Map.lookup li (idents s) )

putVar :: Ident -> Type -> TCM ()
putVar li@(Ident i) t  = modify (\s -> Env (Map.insert li t (idents s)) (functions s)
								(currFun s)) 

getFunType :: Ident -> TCM (Maybe FunType)
getFunType li = gets (\s -> Map.lookup li (functions s) )

putFun :: Ident -> FunType -> TCM ()
putFun li@(Ident i) ft  = modify (\s -> Env (idents s) (Map.insert li ft (functions s)) 
								(currFun s)) 

setCurrFun :: Ident -> TCM ()
setCurrFun ident = modify (\s -> Env (idents s) (functions s) (Just ident)) 

getCurrFunName :: TCM (Maybe Ident)
getCurrFunName = gets currFun 

getCurrFunRetType :: TCM (Maybe Type)
getCurrFunRetType = do
	fName <- getCurrFunName
	case fName of
		Nothing -> return Nothing
		Just fName' -> do
			ft <- getFunType fName'
			case ft of
				Just (Fun t _) -> return $ Just t
				_ -> throwError "Unkown error."

getEnv :: TCM(Env)
getEnv = get

restoreEnv :: Env -> TCM()
restoreEnv s' = put s'

getArgsTypes :: [Arg] -> TCM ([Type])
getArgsTypes [] = return []
getArgsTypes ((Arg t _):at) = do
			ts <-(getArgsTypes at)
			return (t:ts)

insertArgs :: [Arg] -> TCM ()
insertArgs args = insertArgsRec args []
				where
					-- ident list is used to check if we declare
					-- more then 1 var with the same ident
					insertArgsRec :: [Arg] -> [Ident] -> TCM() 
					insertArgsRec [] _ = return ()
					insertArgsRec ((Arg t ident):as) is = do
						if elem ident is
							then throwError $ "errorT13 ident"
						else do
							putVar ident t
							insertArgsRec as (ident:is)
							

throw_error :: String -> TCM a 
throw_error s = do
		maybe_fName <- getCurrFunName
		case maybe_fName of
			Just (Ident i) -> throwError $ s ++ "\nin function " ++ i ++ "."
			Nothing -> throwError s