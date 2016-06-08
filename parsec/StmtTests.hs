module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Ast
import Runtime

assign = Assign "toto" (IntConst 42)

test1 = TestCase  $ (assertEqual "toto := 42" ([("toto",42)]) (evalStmt assign []))


tests = hUnitTestToTests $ TestList [TestLabel "assign test" test1]

main = defaultMain tests 