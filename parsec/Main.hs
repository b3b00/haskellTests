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
import Compiler
import Machine
import SemanticChecker
import Runner
import Runtime




{-
    ******* MAIN *******
-}

--

printHeap :: Machine -> IO()
printHeap machine = do    
    putStrLn ""
    putStrLn "--- HEAP ---"
    putStrLn ""
    putStrLn (dumpHeap machine)
    putStrLn ""
    putStrLn "--- HEAP ---"
    putStrLn ""


action :: String -> String -> Stmt -> IO()
action act out ast = case act of 
    --"-run" -> (run ast)
   {- "-compile" -> do 
        
            let checked = semanticCheck ast in
                if length checked > 0 then          
                    putStrLn ("semantic chec failed "++(show checked))
                else 
                    let compiled = (compileAst ast (Machine 0 [] [] [] []) ) in
                        putStrLn ("compilation suceeded : "++(show ast))-}
    "-compileAndRun" -> do        
            let checked = semanticCheck ast in
                if length checked > 0 then            
                    putStrLn ("semantic chec failed "++(show checked))  
                else 
                    let compiled = (compileAst ast (Machine 0 [] [] [] []) ) in
                        let result = runIt compiled in
                            do
                                putStrLn "done :: "
                                printHeap result
    "-compile" -> do
            let checked = semanticCheck ast in
                 if length checked > 0 then            
                            putStrLn ("semantic chec failed "++(show checked))  
                        else 
                            let compiled = (compileAst ast (Machine 0 [] [] [] []) ) in
                                let serialized  = (show compiled) in
                                    do                                    
                                        writeFile out serialized
                                        putStrLn ("compilation done to "++out)
                                    
    "-i" -> do
        let checked = semanticCheck ast in
                if length checked > 0 then            
                    putStrLn ("semantic chec failed "++(show checked))  
                else 
                    do 
                        run ast

runFile :: String -> IO()
runFile machineSerial  = do
    let machine = read machineSerial  in
        let result = runIt machine in 
            do
                putStrLn "done :: "
                printHeap result
main = do
    args <- getArgs        
    case (args!!0) of
        "-run" -> do 
            putStrLn "running..."
            readFile (args!!1) >>= runFile
            putStrLn ("deserialize and run "++(args!!1))
        otherwise -> do
            putStrLn "generic AST action"
            (parseFile (args!!1)) >>= action (args !! 0) (args!!2)
    

tac  = unlines . reverse . lines

parseA ["-h"] = usage   >> exit
parseA ["-v"] = version >> exit
parseA []     = getContents
parseA fs     = concat `fmap` mapM readFile fs

usage   = putStrLn "Usage: MyParsec [-compile | -i] [file ..]"
version = putStrLn "MyParsec 1.0"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)