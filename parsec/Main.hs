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


dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("-r", runBC)
            , ("-c", compile)
            , ("-i", interprete)
            , ("-cr", compileAndRun)
            ]

{- run ByteCode -}
runBC :: [String] -> IO()
runBC args = do 
            putStrLn "running..."
            readFile (args!!0) >>= runFile
            

runFile :: String -> IO()
runFile machineSerial  = do        
        let machine = loadMachine machineSerial 
        putStrLn("machine loaded...")
        result <- runIt machine   
        printHeap result       
                
{- compile to byetcode -}

loadMachine :: String -> Machine
loadMachine serialized = read serialized

compile :: [String] -> IO()
compile args = 
    do 
        ast <-  (parseFile (args!!0))   
        putStrLn (show ast)
        let out = (args!!1) 
            checked = semanticCheck ast 
        if length checked > 0 then            
            putStrLn ("semantic chec failed "++(show checked))  
        else        
            do         
                let compiled = (compileAst ast (Machine 0 [] [] [] []) ) 
                    serialized  = (show compiled) 
                writeFile out serialized
                putStrLn ("compilation done to "++out)

compileAndRun :: [String] -> IO()
compileAndRun args =       
    do
        ast <- (parseFile (args !! 0))  
             
        let checked = semanticCheck ast 
        {-if length checked > 0 then            
            putStrLn ("semantic chec failed "++(show checked))  
        else-}
        putStrLn("checked :: "++(show checked))
        let compiled = (compileAst ast (Machine 0 [] [] [] []) )
        putStrLn("compiled :: "++(show compiled))
        putStrLn ""
        putStrLn "---"
        putStrLn (printAssemblyBC (bytecode compiled))
        --result <- runIt compiled 
        putStrLn "done :: "
        --printHeap result

{- interprete -}                            
interprete :: [String] -> IO()
interprete args =
    do 
        ast <-  (parseFile (args!!0))   
        let checked = semanticCheck ast in
                if length checked > 0 then            
                   putStrLn ("semantic chec failed "++(show checked))  
                else 
                   run ast






main = do
    (command:args) <- getArgs
    putStrLn ("MAIN :: "++command++" // "++(show args))
    let (Just action) = lookup command dispatch
    action args
    

tac  = unlines . reverse . lines

parseA ["-h"] = usage   >> exit
parseA ["-v"] = version >> exit
parseA []     = getContents
parseA fs     = concat `fmap` mapM readFile fs

usage   = putStrLn "Usage: MyParsec [-compile | -i] [file ..]"
version = putStrLn "MyParsec 1.0"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)