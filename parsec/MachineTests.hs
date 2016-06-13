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


-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [TestLabel "expr const int 1" test1,TestLabel "expr const bool 2" test2]

main = defaultMain tests          
        