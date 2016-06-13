module Main where 

import Ast
import Stack
import Compiler
import Machine
import Runner
import Debug.Trace (trace)


main =     
    let initial = (trace "compilation ...") (compileStmt (Assign "toto" (IntConst 42)) (Machine [] [] [] [])) in
        let final =  (trace ("running :: "++(show initial))) runMachine 0 initial in 
        do
            putStrLn "done."
            putStrLn $ show final
    --putStrLn (show (compileStmt (Assign "toto" (IntConst 42)) (Machine [] [] [] [])))
    --putStrLn (show (compileAExpr (IntConst 1) (Machine [] [] [] [])))
    --putStrLn (show (compileBExpr (BoolConst False) (Machine [] [] [] [])))
    --putStrLn (show (compileStmt (Assign "toto" (IntConst 42)) (Machine [] [] [] [])))
    