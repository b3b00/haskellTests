module Main where 

import Ast
import Stack
import Compiler
import Machine


main = do
    putStrLn (show (compileAExpr (IntConst 1) (Machine [] [] [] [])))
    putStrLn (show (compileBExpr (BoolConst False) (Machine [] [] [] [])))
