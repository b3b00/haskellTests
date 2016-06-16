module Main where 

import Ast
import Stack
import Compiler
import Machine
import Runner
import Debug.Trace (trace)


testCompileAndRun1 ::  IO ()
testCompileAndRun1 = 
    let initial = (trace "compilation ...") (compileStmt (Assign "toto" (IntConst 42)) (Machine 0 [] [] [] [])) in
        let final =  (trace ("running :: "++(show initial))) runMachine initial in 
        do
            putStrLn "done."
            putStrLn $ show final

testCompileAndRunAssignBinary ::  IO ()
testCompileAndRunAssignBinary = 
    let initial = (trace "compilation assign binary op ...") (compileStmt (Assign "toto" (ABinary Substract (IntConst 2) (IntConst 1))) (Machine 0 [] [] [] [])) in
        let final =  (trace ("running :: "++(show initial))) runMachine initial in 
        do
            putStrLn "done."
            putStrLn $ show final

-- *********************************

testCompileSimpleAssign :: IO()
testCompileSimpleAssign = putStrLn (show (compileStmt (Assign "toto" (IntConst 42)) (Machine 0 [] [] [] [])))

-- ********************************* 

testCompileIntConst :: IO()
testCompileIntConst = putStrLn (show (compileAExpr (IntConst 1) (Machine 0 [] [] [] [])))

testCompileIntBinary :: IO()
testCompileIntBinary = putStrLn (show (compileAExpr (ABinary Add (IntConst 2) (IntConst 1)) (Machine 0 [] [] [] [])))

-- ********************************* 

testCompileBoolConst :: IO()
testCompileBoolConst = putStrLn (show (compileBExpr (BoolConst False) (Machine 0 [] [] [] [])))

testCompileBoolOp :: IO()
testCompileBoolOp = putStrLn (show (compileBExpr (BBinary And (BoolConst False) (BoolConst True)) (Machine 0 [] [] [] [])))

-- ********************************* 

testCompileAndRunAdd :: IO()
testCompileAndRunAdd = 
    let ast = (compileAExpr (ABinary Add (IntConst 30) (IntConst 12)) (Machine 0 [] [] [] [])) in
        let eval =  runMachine ast in
            putStrLn ("evaluation result :: "++(show eval ))

main =     
    testCompileBoolOp
    
    