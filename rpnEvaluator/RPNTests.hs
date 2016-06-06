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
import Control.Exception
import Control.Monad    
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import RPN




--test2:: Test
testPlus = TestCase $ (assertEqual "for (computeRPN \"2 2 +\")" (4.0) ((computeRPN "2 2 +")))

testComplex = TestCase $ (assertEqual "for (computeRPN 2 2 + 3 * 4 -)" (8.0) ((computeRPN "2 2 + 3 * 4 -")))

testSum = TestCase $ ( assertEqual "for (computeRPN 3 3 3 3 S)" (12.0) ((computeRPN "3 3 3 3 S"))) 


testFactorial = TestCase $ ( assertEqual "for (computeRPN 5.0 !)" (5.0*4.0*3.0*2.0*1.0) ((computeRPN "5.0 !"))) 

testSQRT = TestCase $ ( assertEqual "for (computeRPN 144.0 sqrt)" (12.0) ((computeRPN "144.0 sqrt"))) 

testPower = TestCase $ ( assertEqual "for (computeRPN 2.0 2.0 ^)" (4.0) ((computeRPN "2.0 2.0 ^"))) 

testPi = TestCase $ ( assertEqual "for (computeRPN PI)" (pi) ((computeRPN "PI"))) 

testLn = TestCase $ ( assertEqual "for (computeRPN  e ln)" (1.0) ((computeRPN "e ln"))) 

--test3 = TestCase $ ( assertException ErrorCall (evaluate $ computeRPN " 2 2 X"))



-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [TestLabel "testPlus" testPlus,TestLabel "testComplex" testComplex, TestLabel "testSum" testSum,TestLabel "testFactorial" testFactorial,TestLabel "testSQRT" testSQRT,TestLabel "testPower" testPower,TestLabel "testPi" testPi, TestLabel "testLn" testLn]

main = defaultMain tests          
        