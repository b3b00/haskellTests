
module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Stack

stack1 = [IntVal 1, IntVal 2, StrVal "coucou"]

test1 = TestCase  $ (assertEqual "peek [1,2,coucou]" (StrVal "coucou") (peekValue stack1))

stack2 = []

test2 = TestCase  $ (assertEqual "peek []" (NullVal) (peekValue stack2))

stack3 = [IntVal 1, IntVal 2]

test3 = TestCase  $ (assertEqual "pop [1,2]" ((IntVal 2),[IntVal 1]) (popValue stack3))

stack4 = []

test4 = TestCase  $ (assertEqual "pop []" (NullVal,[]) (popValue stack4))

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [TestLabel "peek 1" test1, TestLabel "peek null" test2, TestLabel "pop 1" test3, TestLabel "pop null" test4]

main = defaultMain tests          
        