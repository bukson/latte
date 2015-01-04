module IntermediatePartition where

--
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe(fromJust)
import TAC


type PM a = State Env a

data Env = Env {
	currentBlock :: [Tac],
	blockList :: [Tac],
	currentFun :: Tac, -- FunLabel fl
	dump :: Map.Map String Address,
	code :: [Tac]
}

runPM :: PM a ->  a
runPM m  = evalState m (Env [] [] (FunLabel (Lab "__null")) Map.empty [])

partition :: [Tac] -> [Tac]
partition insL = runPM (refactor $ insL)

formBlockFromCurrent :: PM Tac
formBlockFromCurrent = do
	currB <- gets currentBlock
	let r_currB = reverse currB
	let (Label l) = head r_currB
	return $ Blck l (tail r_currB)

newBloc :: Address -> PM ()
newBloc newL = do
	currB <- gets currentBlock
	if null currB then 
		modify $ \s -> s {currentBlock = [Label newL]}
	else do
		currB' <- formBlockFromCurrent 	
		modify $ \s -> s {blockList = (currB'):(blockList s)}
		modify $ \s -> s {currentBlock = [Label newL]}
	return ()

newFun :: Address -> PM ()
newFun newFl = do
	currFun@(FunLabel oldFl) <- gets currentFun -- FunLabel fl
	currB <- gets currentBlock
	if null currB then
		modify $ \s -> s {currentFun = (FunLabel newFl)} 
	else do 
		currB' <- formBlockFromCurrent 
		blockL <- gets blockList
		modify $ \s -> s {code = (Fun oldFl (reverse (currB':blockL)):(code s))}
		modify $ \s -> s {blockList = []}
		modify $ \s -> s {currentFun = (FunLabel newFl)}
	return ()

swapBlocs :: PM ()
swapBlocs = do
	(b1:bL) <- gets blockList
	currB <- gets currentBlock
	currB' <- formBlockFromCurrent 
	modify $ \s -> s {blockList = (b1:currB':bL)}
	modify $ \s -> s {currentBlock = []}
	return ()

insert :: Tac -> PM ()
insert nextI = do
	modify $ \s -> s{currentBlock = nextI : (currentBlock s)}
	return ()

makeFi :: Address -> String -> [(Address, Address)] -> PM ()
makeFi a s ((l1, a1):(l2, a_unknow):[]) = do
	m <- gets dump
	insert $ Dump m
	let maybe_a_known = Map.lookup s m
	let fi = Fi a ((l1,a1):(l2, fromJust maybe_a_known):[])
	return ()


refactor :: [Tac] -> PM [Tac]
refactor [] = do
	FunLabel fl <- gets currentFun
	newFun fl 
	insL <- gets code
	return $ reverse insL
refactor (nextI:insL) = do
	case nextI of
		Label l -> newBloc l
		FunLabel l -> newFun l
		ChangeBlocs -> swapBlocs 
		_ -> insert nextI
	refactor insL


