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
	fCLabel :: Integer,
	constants :: Map.Map Address String,
	lastLabel :: Address,
	code :: [Tac]
}

emptyEnv = Env Map.empty 0 0 0 Map.empty (Lab "main") []

runIM :: IM a -> a
runIM m = evalState m emptyEnv

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

insertIdent :: Ident -> IM()
insertIdent i = modify $ \s -> s {idents = (Map.insert i 0 (idents s))}

insertArgs :: [Arg] -> IM()
insertArgs args = mapM_ (\a@(Arg _ i) -> insertIdent i) args

getInstance :: Ident -> IM Address
getInstance ident = do
	Just n <- gets $ \s -> Map.lookup ident (idents s)
	return $ makeAddress ident n

getInstanceNumber :: Ident -> IM Integer
getInstanceNumber i = do
	maybe_n <- gets $ \s -> Map.lookup i (idents s)
	return $ fromJust maybe_n

freshInstance :: Ident -> IM Address
freshInstance ident = do
	Just n <- gets $ \s -> Map.lookup ident (idents s)
	modify $ \s -> s {idents = (Map.insert ident (n + 1) (idents s))}
	return $ makeAddress ident (n + 1)

makeAddress :: Ident -> Integer -> Address
makeAddress (Ident s) i = Address s i

getEnv :: IM Env 
getEnv = get

getIdents :: IM (Map.Map Ident Integer)
getIdents = gets idents

setIdents :: (Map.Map Ident Integer) -> IM ()
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

genFi :: Map.Map Ident Integer -> Address -> Map.Map Ident Integer -> Address -> IM ()
genFi instances1 l1 instances2 l2 = 
	mapM_
	(\(ident,_) -> genFiOne ident (Map.lookup ident instances1) l1
					(fromJust $ Map.lookup ident instances2) l2) 
	(Map.toList instances2)
	where
		genFiOne :: Ident -> (Maybe Integer) -> Address -> Integer -> Address -> IM ()
		genFiOne ident maybe_i1 l1 i2 l2 = do			
			case maybe_i1 of
				Just i1 -> do
					if i1 == i2 then
						return ()
					else do
						a <- freshInstance ident
						emit $ Fi a [(l1, makeAddress ident i1), (l2, makeAddress ident i2)]
				Nothing -> return ()

genTmpFi :: Map.Map Ident Integer -> Address -> Address -> IM ()
genTmpFi instances1 l1 l2 = do
	instances <- getIdents
	mapM_ (\(ident, i) -> genTmp ident (fromJust $ Map.lookup ident instances1)) (Map.toList instances)
	return ()
	where
		genTmp :: Ident -> Integer -> IM ()
		genTmp ident@(Ident i) i1 = do
			fresh <- freshInstance ident
			emit $ TmpFi fresh i [(l1, makeAddress ident i1),(l2, l2)]

genDump :: IM ()
genDump = do
	instances <- getIdents
	let instances' = map (\((Ident i), n) -> (i, makeAddress (Ident i) n)) (Map.toList instances)
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
			FunLabel l -> 
				if null currentBlock then
					partition instructions [] (nextI:code)
				else
					let b = Blck (reverse currentBlock)
					in partition instructions [] (nextI:b:code)
			ChangeBlocs ->
					let b = Blck (reverse currentBlock)
					in partition instructions [] ((head code):b:(tail code))
			_ -> partition instructions (nextI:currentBlock) code
