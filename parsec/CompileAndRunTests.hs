module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Ast
import Stack
import Machine
import Compiler
import Parser
import Runner


compileAndRun :: String -> Machine
compileAndRun prog =
    let ast = parseString prog in
    let initial =  (compileStmt ast (Machine 0 [] [] [] [])) in
        let final =  runMachine initial in 
            final

getFinalVariableValue :: String -> String -> StackValue 
getFinalVariableValue prog name = (getHeapValue name (compileAndRun prog))


testCompileAndRun label prog expectName expectValue = 
    TestCase $ (assertEqual label expectValue (getFinalVariableValue prog expectName))

p1 = "( toto := 2 - 1; skip; toto := -42 )"
e1 = IntVal (-42)
l1 = "int assign"
test1 = testCompileAndRun l1 p1 "toto" e1

p2 = "( toto := -1; if (true) then (skip;toto := 42) else (toto := 0; skip); print 1789; print toto )"
e2 = IntVal (42)
l2 = "if then else"
test2 = testCompileAndRun l2 p2 "toto" e2

p3 = "( toto := -1; while toto < 10 do (print toto; toto := toto +1 ); print toto)"
e3 = IntVal (10)
l3 = "while"
test3 = testCompileAndRun l3 p3 "toto" e3


        



tests = hUnitTestToTests $ TestList [TestLabel l1 test1,TestLabel l2 test2,TestLabel l3 test3]

main = defaultMain tests          
        