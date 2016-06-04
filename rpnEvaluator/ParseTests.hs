{-module RPNTests where

import RPN(splitExpression)
import RPN(computeRPN)
import Test.HUnit

testSplitExpression = (splitExpression (words " 2 2 +")) == (["2","2"],["+"])    

test1 :: Test
test1 = TestCase (assertEqual "for (splitExpression 2 2 +)" (["2","2"],["+"]) (splitExpression (words " 2 2 +")))

test2:: Test
test2 = TestCase (assertEqual "for (computeRPN \"2 2 +\")" (4.0) (computeRPN "2 2 +"))



tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]


main :: IO Counts
main =  runTestTT tests-}
module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import RPN


testSplitExpression = (splitExpression (words " 2 2 +")) == (["2","2"],["+"])    

--test1 :: Test
test1 = TestCase  $ (assertEqual "for (splitExpression 2 2 +)" (["2","2"],["+"]) (splitExpression (words " 2 2 +")))

test2 = TestCase $ (assertEqual "split (3 3 3  3 S)" (["3","3","3","3"],["S"]) (splitExpression (words "  3 3 3 3  S")))



-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [TestLabel "test1" test1, TestLabel "test2" test2]

main = defaultMain tests          
        