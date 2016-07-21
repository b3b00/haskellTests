
module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Ast
import Runtime
import Text.Parsec.Pos   

pos = (newPos "dummy" 0 0)


tAndt = Binary And pos (BoolConst pos True) (BoolConst pos True)

test1 = TestCase  $ (assertEqual "true and true" (BoolV True) (evalExpr tAndt []))

notT = Not pos (BoolConst pos True) 

test2 = TestCase  $ (assertEqual "not true" (BoolV False) (evalExpr notT []))

tOrf = Binary Or pos (BoolConst pos True) (BoolConst pos False)

test3 = TestCase  $ (assertEqual "true and true" (BoolV True) (evalExpr tOrf []))


greater = Binary Greater pos (IntConst pos 1) (IntConst pos 2)

test4 = TestCase  $ (assertEqual "1 > 2" (BoolV False) (evalExpr greater []))

less = Binary Lesser pos (IntConst pos 1) (IntConst pos 2)

test5 = TestCase  $ (assertEqual "1 < 2" (BoolV True) (evalExpr less []))

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [TestLabel "and test" test1, TestLabel "not test" test2, TestLabel "or test" test3, TestLabel "greater test" test4, TestLabel "less test" test5]

main = defaultMain tests          
        