module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Ast
import Stack
import Machine
import Compiler
import Text.Parsec.Pos   

pos = (newPos "dummy" 0 0)

expr1 = (IntConst pos 1)

test1 = TestCase  $ (assertEqual "const int 1" (Machine 0 [1,0] [] [IntVal 1] []) (compileExpr expr1 (Machine 0 [] [] [] [])))

expr2 = (BoolConst pos True)

test2 = TestCase  $ (assertEqual "const bool true" (Machine 0 [1,0] [] [BoolVal True] []) (compileExpr expr2 (Machine 0 [] [] [] [])))

expr3 = (Binary Add pos (IntConst pos 2) (IntConst pos 1))
test3 = TestCase  $ (assertEqual "int binary op 2+1" (Machine 0 [1,0,1,1,4] [] [IntVal 2, IntVal 1] []) (compileExpr expr3 (Machine 0 [] [] [] [])))

expr4 = (Binary And pos (BoolConst pos False) (BoolConst pos True)) 
test4 = TestCase  $ (assertEqual "int binary op False and true" (Machine 0 [1,0,1,1,10] [] [BoolVal False, BoolVal True] []) (compileExpr expr4 (Machine 0 [] [] [] [])))


tests = hUnitTestToTests $ TestList [TestLabel "expr const int 1" test1,TestLabel "expr const bool true" test2,TestLabel "expr binary int 2+1" test3,TestLabel "expr binary bool False and True" test4]

main = defaultMain tests          
        