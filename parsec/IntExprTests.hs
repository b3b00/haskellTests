
module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Ast
import Runtime

twoPlusTwo = ABinary Add (IntConst 2) (IntConst 2)

test1 = TestCase  $ (assertEqual "2+2" (4) (fst (evalAExpr twoPlusTwo [])))

fiveDivTwo = ABinary Divide (IntConst 5) (IntConst 2)

test2 = TestCase  $ (assertEqual "5/2" (2) (fst (evalAExpr fiveDivTwo [])))

fiveMinusTwo = ABinary Substract (IntConst 5) (IntConst 2)

test3 = TestCase  $ (assertEqual "5-2" (3) (fst (evalAExpr fiveMinusTwo [])))

fiveTimesTwo = ABinary Multiply (IntConst 5) (IntConst 2)

test4 = TestCase  $ (assertEqual "5*2" (10) (fst (evalAExpr fiveTimesTwo [])))

variable = Var "toto"

test5 = TestCase  $ (assertEqual "toto" (10) (fst (evalAExpr variable [("toto",10)])))

{-

notT = Not (BoolConst True) 

test2 = TestCase  $ (assertEqual "not true" (False) (fst (evalBExpr notT [])))

tOrf = BBinary Or (BoolConst True) (BoolConst False)

test3 = TestCase  $ (assertEqual "true and true" (True) (fst (evalBExpr tOrf [])))


greater = RBinary Greater (IntConst 1) (IntConst 2)

test4 = TestCase  $ (assertEqual "1 > 2" (False) (fst (evalBExpr greater [])))

less = RBinary Less (IntConst 1) (IntConst 2)

 test5 = TestCase  $ (assertEqual "1 < 2" (True) (fst(evalBExpr less []))) -}

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [TestLabel "add test" test1, TestLabel "div test" test2, TestLabel "substract test" test3, TestLabel "multiplication test" test4, TestLabel "avraible test" test5]
--, TestLabel "not test" test2, TestLabel "or test" test3, TestLabel "greater test" test4, TestLabel "less test" test5]

main = defaultMain tests          
        