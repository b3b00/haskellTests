
module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Ast
import Runtime
import Text.Parsec.Pos   

pos = (newPos "dummy" 0 0)

twoPlusTwo = Binary Add pos (IntConst pos 2) (IntConst pos 2)

test1 = TestCase  $ (assertEqual "2+2" (IntegerV 4) (evalExpr twoPlusTwo []))

fiveDivTwo = Binary Divide pos (IntConst pos 5) (IntConst pos 2)

test2 = TestCase  $ (assertEqual "5/2" (IntegerV 2) (evalExpr fiveDivTwo []))

fiveMinusTwo = Binary Substract pos (IntConst pos 5) (IntConst pos 2)

test3 = TestCase  $ (assertEqual "5-2" (IntegerV 3) (evalExpr fiveMinusTwo []))

fiveTimesTwo = Binary Multiply pos (IntConst pos 5) (IntConst pos 2)

test4 = TestCase  $ (assertEqual "5*2" (IntegerV 10) (evalExpr fiveTimesTwo []))

variable = Var pos "toto"

test5 = TestCase  $ (assertEqual "toto" (IntegerV 10) (evalExpr variable [("toto",IntegerV 10)]))


-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [TestLabel "add test" test1, TestLabel "div test" test2, TestLabel "substract test" test3, TestLabel "multiplication test" test4, TestLabel "avraible test" test5]
--, TestLabel "not test" test2, TestLabel "or test" test3, TestLabel "greater test" test4, TestLabel "less test" test5]

main = defaultMain tests          
        