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
	idents :: Map.Map Ident (String, Integer),
	fTemp :: Integer, 
	fLabel :: Integer,
	fCLabel :: Integer,
	constants :: Map.Map Address String,
	lastLabel :: Address,
	funs :: Map.Map Ident FunType,
	currFunRetType :: Type,
	code :: [Tac]
}

emptyEnv = Env Map.empty 0 0 0 Map.empty (Lab "main") Map.empty Void []

runIM :: IM a -> a
runIM m = evalState m emptyEnv

setCurrFunRetType :: Type -> IM ()
setCurrFunRetType t = do
	modify $ \s -> s {currFunRetType = t}
	return ()

getCurrFunRetType :: IM Type
getCurrFunRetType = gets currFunRetType

freshTemp :: IM Address
freshTemp = do
		fresh <- gets fTemp
		modify $ \s -> s {fTemp = fresh + 1}
		return $ Address "_t" (fresh + 1)

freshLabel :: IM Address
freshLabel = do
		fresh <- gets fLabel
		modify $ \s -> s {fLabel = fresh + 1}
		return $ Lab ("L" ++ (show (fresh + 1)))

freshCLabel :: IM Address
freshCLabel = do
		fresh <- gets fCLabel
		modify $ \s -> s {fCLabel = fresh + 1}
		return $ Lab ("C" ++ (show (fresh + 1)))

insertConstant :: String -> IM(Address)
insertConstant str = do
		flabel <- freshCLabel
		modify $ \s -> s {constants = (Map.insert flabel str (constants s))}
		return flabel

getConstants :: IM [(Address, String)]
getConstants = do
	constMap <- gets constants
	return $ Map.toList constMap

insertFuns :: (Map.Map Ident FunType) -> IM ()
insertFuns fs = do
	modify $ \s -> s {funs = fs}
	return ()

getFunRetllvmType :: Ident -> IM String
getFunRetllvmType ident = do
	Just (Fun ret_t _) <- gets $ \s -> Map.lookup ident (funs s)
	return $ tollvmType ret_t

getFunArgsllvmType :: Ident -> IM [String]
getFunArgsllvmType ident = do
	Just (Fun _ types) <- gets $ \s -> Map.lookup ident (funs s)
	return $ map tollvmType types

insertIdent :: Ident -> Type -> IM()
insertIdent i t = modify $ \s -> s {idents = (Map.insert i (tollvmType(t),0) (idents s))}

insertArgs :: [Arg] -> IM()
insertArgs args = mapM_ (\a@(Arg t i) -> insertIdent i t) args

getInstance :: Ident -> IM Address
getInstance ident = do
	Just (_,n) <- gets $ \s -> Map.lookup ident (idents s)
	return $ makeAddress ident n

getType :: Ident -> IM String
getType ident = do
	Just (typ,_) <- gets $ \s -> Map.lookup ident (idents s)
	return typ

getInstanceNumber :: Ident -> IM Integer
getInstanceNumber i = do
	Just (_,n)  <- gets $ \s -> Map.lookup i (idents s)
	return n

freshInstance :: Ident -> IM Address
freshInstance ident = do
	Just (typ,n) <- gets $ \s -> Map.lookup ident (idents s)
	modify $ \s -> s {idents = (Map.insert ident (typ,(n + 1)) (idents s))}
	return $ makeAddress ident (n + 1)

makeAddress :: Ident -> Integer -> Address
makeAddress (Ident s) i = Address s i

getEnv :: IM Env 
getEnv = get

getIdents :: IM (Map.Map Ident (String,Integer))
getIdents = gets idents

setIdents :: (Map.Map Ident (String,Integer)) -> IM ()
setIdents i = modify $ \s -> s{idents = i }

getCode :: IM [Tac]
getCode = gets code

emit :: Tac -> IM ()
emit instruction = do
	--c <- getCode
	modify $ \s -> s {code = instruction:(code s)}
	--modify(\s -> Env (idents s) (fTemp s) (fLabel s) (fCLabel s)
	--					(constants s) (used s) (instruction:c))

setLabel :: Address -> IM ()
setLabel l = modify $ \s -> s {lastLabel = l}

getLastLabel :: IM Address
getLastLabel = gets lastLabel

-- restoreEnv saves changes in code and Constant Labels counter
restoreEnv :: Env -> IM()
restoreEnv s' = do
	let ids = idents s'
	modify $ \s -> s {idents = ids}
	return ()

genFi :: Map.Map Ident (String,Integer) -> Address -> Map.Map Ident (String,Integer) -> Address -> IM ()
genFi instances1 l1 instances2 l2 = 
	mapM_
	(\(ident,_) -> genFiOne ident (Map.lookup ident instances1) l1
					(fromJust $ Map.lookup ident instances2) l2) 
	(Map.toList instances2)
	where
		genFiOne :: Ident -> Maybe (String,Integer) -> Address -> (String,Integer) -> Address -> IM ()
		genFiOne ident maybe_i1 l1 (typ2,i2) l2 = do			
			case maybe_i1 of
				Just (typ,i1) -> do
					if i1 == i2 then
						return ()
					else do
						a <- freshInstance ident
						a_type <- getType ident
						emit $ Fi a a_type [(l1, makeAddress ident i1), (l2, makeAddress ident i2)]
				Nothing -> return ()

genTmpFi :: Map.Map Ident (String,Integer) -> Address -> Address -> IM ()
genTmpFi instances1 l1 l2 = do
	instances <- getIdents
	mapM_ (\(ident, i) -> genTmp ident (fromJust $ Map.lookup ident instances1)) (Map.toList instances)
	return ()
	where
		genTmp :: Ident -> (String,Integer) -> IM ()
		genTmp ident@(Ident i) (typ1,i1) = do
			fresh <- freshInstance ident
			typ <- getType ident
			emit $ TmpFi fresh typ i [(l1, makeAddress ident i1),(l2, l2)]

genDump :: IM ()
genDump = do
	instances <- getIdents
	let instances' = map (\((Ident i), (typ,n)) -> (i, makeAddress (Ident i) n)) (Map.toList instances)
	emit $ Dump (Map.fromList instances')

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

-- negative operation
negRelOp :: Op -> Op
negRelOp TAC.LTH = TAC.GE
negRelOp TAC.LE = TAC.GTH
negRelOp TAC.GTH = TAC.LE
negRelOp TAC.GE = TAC.LTH
negRelOp TAC.EQU = TAC.NE
negRelOp TAC.NE = TAC.EQU

blockPartition :: [Tac] -> [Tac]
blockPartition code = partition code [] []
	where
		partition :: [Tac] -> [Tac] -> [Tac] -> [Tac]
		partition [] [] code = reverse code
		partition [] currentBlock code = 
			let b = Blck (reverse currentBlock)
			in reverse (b:code)
		partition (nextI:instructions) currentBlock code = case nextI of
			Label l -> 
				if null currentBlock then
					partition instructions [Label l] code
				else
					let b = Blck (reverse currentBlock)
					in partition instructions [Label l] (b:code) 
			FunLabel l _ _ -> 
				if null currentBlock then
					partition instructions [] (nextI:code)
				else
					let b = Blck (reverse currentBlock)
					in partition instructions [] (nextI:b:code)
			ChangeBlocs ->
					let b = Blck (reverse currentBlock)
					in partition instructions [] ((head code):b:(tail code))
			_ -> partition instructions (nextI:currentBlock) code


tollvmType :: Type -> String
tollvmType t = case t of
	Int -> "i32"
	Str -> "i8*"
	Bool -> "i1"
	Void -> "void" 

tollvmArgs :: [Arg] -> [(String,Address)]
tollvmArgs args = map tollvm args
	where 
		tollvm :: Arg -> (String,Address)
		tollvm (Arg t (Ident i)) = ((tollvmType t), Address i 0) 