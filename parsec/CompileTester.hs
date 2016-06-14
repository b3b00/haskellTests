module Main where 

import Ast
import Stack
import Compiler
import Machine
import Runner
import Debug.Trace (trace)


testCompileAndRun1 ::  IO ()
testCompileAndRun1 = 
    let initial = (trace "compilation ...") (compileStmt (Assign "toto" (IntConst 42)) (Machine [] [] [] [])) in
        let final =  (trace ("running :: "++(show initial))) runMachine 0 initial in 
        do
            putStrLn "done."
            putStrLn $ show final



testCompileSimpleAssign :: IO()
testCompileSimpleAssign = putStrLn (show (compileStmt (Assign "toto" (IntConst 42)) (Machine [] [] [] [])))

testCompileIntConst :: IO()
testCompileIntConst = putStrLn (show (compileAExpr (IntConst 1) (Machine [] [] [] [])))

testCompileIntBinary :: IO()
testCompileIntBinary = putStrLn (show (compileAExpr (ABinary Add (IntConst 2) (IntConst 1)) (Machine [] [] [] [])))

testCompileBoolConst :: IO()
testCompileBoolConst = putStrLn (show (compileBExpr (BoolConst False) (Machine [] [] [] [])))



main =     
    testCompileIntBinary
    
    