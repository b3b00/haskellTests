module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Ast
import Runtime
import Parser

assign = parseString "( toto := 42 )"

test1 = TestCase  $ (assertEqual "toto := 42" ([("toto",IntegerV 42)]) (evalStmt assign []))

manyAssign = parseString "( toto := 1; titi := 2 )"

test2 = TestCase  $ (assertEqual "toto := 1; titi := 2;" ([("titi",IntegerV 2),("toto",IntegerV 1)]) (evalStmt manyAssign []))

ifthenelseTrue = parseString "(   toto := 0;   if (true) then       (toto :=1)     else       (toto := 2) )"


test3 = TestCase  $ (assertEqual "toto := 0; if true then toto :=1; else toto := 2;" ([("toto",IntegerV 1)]) (evalStmt ifthenelseTrue [])) 


ifthenelseEq = parseString "( toto := 0; if (toto == 0) then (toto :=1) else (toto := 2)) "

test4 = TestCase  $ (assertEqual "toto := 0; if toto == 0 then toto :=1; else toto := 2;" ([("toto",IntegerV 1)]) (evalStmt ifthenelseEq []))

while = parseString "toto := 0; while (toto < 10) do (toto := toto + 1)"

test5 = TestCase  $ (assertEqual "toto := 0; while (toto < 10) do (toto := toto + 1)" ([("toto",IntegerV 10)]) (evalStmt while []))

tests = hUnitTestToTests $ TestList [TestLabel "assign test" test1,TestLabel "many assign test" test2,TestLabel "if then else test" test3,TestLabel "if then else test with eq" test4,TestLabel "while" test5]

main = do
    putStrLn "many assigns ::"
    putStrLn (show manyAssign)
    putStrLn "ifthenelseTrue : "
    putStrLn (show ifthenelseTrue)
    putStrLn "ifthenelseEq : "
    putStrLn (show ifthenelseEq)
    putStrLn "while : "
    putStrLn (show while)
    putStrLn("---")
    defaultMain tests 