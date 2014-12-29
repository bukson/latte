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
        	Right _ -> head $ map (\t -> putStrLn $ show t ) (runIM $ generate topDefL)


generate :: [TopDef] -> IM [Tac]
generate [] = do
	code <- getCode
	return $ reverse code
generate (d:ds) = do
	generateTopDef d
	generate ds

generateTopDef :: TopDef -> IM ()
generateTopDef (FnDef _ (Ident s) args (Block stmtL)) = do
	emit (Lab (Label s))
	renv <- IntermediateEnv.getEnv
	insertArgs args
	--generateStmtL
	restoreEnv renv








