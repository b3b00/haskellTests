module Main where

import System.Exit
import System.Environment   
import Data.List  
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


import Parser
import Ast
import Runtime
import Compiler
import Machine




{-
    ******* MAIN *******
-}

--

action :: String -> Stmt -> IO()
action act ast = case act of 
    "-run" -> (run ast)
    "-compile" -> putStrLn (show (compileAst ast (Machine 0 [] [] [] []) ))
    _ -> putStrLn ("unknwon action ["++act++"]")

main = do
    args <- getArgs    
    (parseFile (args!!1)) >>= action (args !! 0) 
    

tac  = unlines . reverse . lines

parseA ["-h"] = usage   >> exit
parseA ["-v"] = version >> exit
parseA []     = getContents
parseA fs     = concat `fmap` mapM readFile fs

usage   = putStrLn "Usage: tac [-vh] [file ..]"
version = putStrLn "Haskell tac 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)