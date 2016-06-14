module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Ast
import Stack
import Machine
import Compiler

expr1 = (IntConst 1)

test1 = TestCase  $ (assertEqual "const int 1" (Machine [1,1] [] [IntVal 1] []) (compileAExpr expr1 (Machine [] [] [] [])))

expr2 = (BoolConst True)

test2 = TestCase  $ (assertEqual "const bool true" (Machine [1,1] [] [BoolVal True] []) (compileBExpr expr2 (Machine [] [] [] [])))

expr3 = (ABinary Add (IntConst 2) (IntConst 1))
test3 = TestCase  $ (assertEqual "int binary op 2+1" (Machine [1,1,1,2,4] [] [IntVal 2, IntVal 1] []) (compileAExpr expr3 (Machine [] [] [] [])))

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [TestLabel "expr const int 1" test1,TestLabel "expr const bool true" test2,TestLabel "expr bianry int 2+1" test3]

main = defaultMain tests          
        