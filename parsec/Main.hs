module Main where

import Parser
import Ast
import Runtime

import System.Exit
import System.Environment   
import Data.List  
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token



{-
    ******* MAIN *******
-}

--
main = do
    ast <- parseFile "test.prog"
    exitCode <- run ast
    putStrLn (show (exitCode)) 

tac  = unlines . reverse . lines

parseA ["-h"] = usage   >> exit
parseA ["-v"] = version >> exit
parseA []     = getContents
parseA fs     = concat `fmap` mapM readFile fs

usage   = putStrLn "Usage: tac [-vh] [file ..]"
version = putStrLn "Haskell tac 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)