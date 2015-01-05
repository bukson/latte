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
import Intermediate(translate)
import CopyO(copyO)
import DeadO(deadO)

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
        	Right funs -> do
        		--mapM_ (\t -> putStrLn $ show t ) (translate topDefL)
        		--mapM_ (\t -> putStrLn $ show t ) (copyO $ translate topDefL)
        		mapM_ (\t -> putStrLn $ show t ) (deadO $ copyO $ translate topDefL funs)
        		putStrLn ""

optimize :: [Tac] -> [Tac]
optimize insL =  deadO $ copyO insL