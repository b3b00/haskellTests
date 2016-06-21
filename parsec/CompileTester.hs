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
    let ast = parseString "( toto := 2 - 1; skip; toto := -42 )" in
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

testIfThenElse :: IO()
testIfThenElse = putStrLn (show (compileIfThenElse (BoolConst True) (Seq [Skip, Print (IntConst 42)]) (Seq [Print (IntConst 0), Skip]) (Machine 0 [] [] [] []) ) )

testCompileAndRunIfThenElse ::  IO ()
testCompileAndRunIfThenElse = 
    let ast = parseString "( toto := -1; if (true) then (skip;toto := 42) else (toto := 0; skip); print 1789; print toto )" in
    let initial = {-trace ("compilation if then else ... of "++(show ast)++"\n")-} (compileStmt ast (Machine 0 [] [] [] [])) in    
        let final = trace ("\nrunning initial :: "++(show initial)) runMachine initial in 
        do
            putStrLn "done."
            printHeap final
            putStrLn $ show final


testCompileAndRunWhile ::  IO ()
testCompileAndRunWhile = 
    let ast = parseString "( toto := -1; while toto < 10 do (print toto; toto := toto +1 ); print toto)" in
    let initial = trace ("compilation while... "++"\n") (compileStmt ast (Machine 0 [] [] [] [])) in    
        let final = {-trace ("\nrunning initial :: "++(show initial)++"\n"++(printAssembly initial)++"\n")-} runMachine initial in 
        do
            putStrLn "done."
            printHeap final
            putStrLn $ show final            


testReplace :: IO()
testReplace = putStrLn (show (replace 3 42 [1..5]))

main =
    testCompileAndRunAssignIntBinary
    {-testCompileAndRunAddInt-}
    {-testCompileAndRunInt-}
    {-testCompileAndRunWhile-}
    {-testCompileAndRunIfThenElse-}
    
    