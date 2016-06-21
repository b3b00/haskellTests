module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Ast
import Stack
import Machine
import Compiler

expr1 = (IntConst 1)

test1 = TestCase  $ (assertEqual "const int 1" (Machine 0 [1,0] [] [IntVal 1] []) (compileAExpr expr1 (Machine 0 [] [] [] [])))

expr2 = (BoolConst True)

test2 = TestCase  $ (assertEqual "const bool true" (Machine 0 [1,0] [] [BoolVal True] []) (compileBExpr expr2 (Machine 0 [] [] [] [])))

expr3 = (ABinary Add (IntConst 2) (IntConst 1))
test3 = TestCase  $ (assertEqual "int binary op 2+1" (Machine 0 [1,0,1,1,4] [] [IntVal 2, IntVal 1] []) (compileAExpr expr3 (Machine 0 [] [] [] [])))

expr4 = (BBinary And (BoolConst False) (BoolConst True)) 
test4 = TestCase  $ (assertEqual "int binary op False and true" (Machine 0 [1,0,1,1,10] [] [BoolVal False, BoolVal True] []) (compileBExpr expr4 (Machine 0 [] [] [] [])))


tests = hUnitTestToTests $ TestList [TestLabel "expr const int 1" test1,TestLabel "expr const bool true" test2,TestLabel "expr binary int 2+1" test3,TestLabel "expr binary bool False and True" test4]

main = defaultMain tests          
        