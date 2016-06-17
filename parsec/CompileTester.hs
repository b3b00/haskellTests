module Main where 

import Ast
import Stack
import Compiler
import Machine
import Runner
import Parser
import Debug.Trace (trace)


printHeap :: Machine -> IO()
printHeap machine = do
    putStrLn "done."
    putStrLn ""
    putStrLn "--- HEAP ---"
    putStrLn ""
    putStrLn (dumpHeap machine)
    putStrLn ""
    putStrLn "--- HEAP ---"
    putStrLn ""

testCompileAndRunInt ::  IO ()
testCompileAndRunInt = 
    let initial = (trace "compilation ...") (compileStmt (AssignA "toto" (IntConst 42)) (Machine 0 [] [] [] [])) in
        let final =  (trace ("running :: "++(show initial))) runMachine initial in             
        do
            putStrLn "done."
            printHeap final
            putStrLn $ show final

testCompileAndRunAssignIntBinary ::  IO ()
testCompileAndRunAssignIntBinary = 
    let ast = parseString "( toto := 2 - 1; skip )" in
    let initial = trace ("compilation assign binary op ... of "++(show ast)) (compileStmt ast (Machine 0 [] [] [] [])) in
        let final = trace ("running :: "++(show initial)) runMachine initial in 
        do
            putStrLn "done."
            printHeap final
            putStrLn $ show final

testCompileAndRunAddInt :: IO()
testCompileAndRunAddInt = 
    let ast = (compileAExpr (ABinary Add (IntConst 30) (IntConst 12)) (Machine 0 [] [] [] [])) in
        let eval =  runMachine ast in
            putStrLn ("evaluation result :: "++(show eval ))            

testCompileAndRunAndBool :: IO()
testCompileAndRunAndBool = 
    let ast = (compileBExpr (BBinary And (BoolConst False) (BoolConst True)) (Machine 0 [] [] [] [])) in
        let eval =  runMachine ast in
            putStrLn ("evaluation result :: "++(show eval ))                 

testCompileAndRunAndAssignBool :: IO()
testCompileAndRunAndAssignBool = 
    let ast = (compileStmt (AssignB "toto" (BBinary And (BoolConst False) (BoolConst True))) (Machine 0 [] [] [] [])) in
        let eval =  runMachine ast in
            putStrLn ("evaluation result :: "++(show eval ))       

-- *********************************

testCompileSimpleAssign :: IO()
testCompileSimpleAssign = putStrLn (show (compileStmt (AssignA "toto" (IntConst 42)) (Machine 0 [] [] [] [])))

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



main =     
    testCompileAndRunAssignIntBinary
    
    