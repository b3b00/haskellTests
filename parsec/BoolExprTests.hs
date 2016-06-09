
module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Ast
import Runtime

tAndt = BBinary And (BoolConst True) (BoolConst True)

test1 = TestCase  $ (assertEqual "true and true" (True) (evalBExpr tAndt []))

notT = Not (BoolConst True) 

test2 = TestCase  $ (assertEqual "not true" (False) (evalBExpr notT []))

tOrf = BBinary Or (BoolConst True) (BoolConst False)

test3 = TestCase  $ (assertEqual "true and true" (True) (evalBExpr tOrf []))


greater = RBinary Greater (IntConst 1) (IntConst 2)

test4 = TestCase  $ (assertEqual "1 > 2" (False) (evalBExpr greater []))

less = RBinary Less (IntConst 1) (IntConst 2)

test5 = TestCase  $ (assertEqual "1 < 2" (True) (evalBExpr less []))

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [TestLabel "and test" test1, TestLabel "not test" test2, TestLabel "or test" test3, TestLabel "greater test" test4, TestLabel "less test" test5]

main = defaultMain tests          
        