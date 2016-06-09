module Compiler where 
import System.IO
import Control.Monad
import Ast
import Machine
import Stack
import Assoc
import Debug.Trace (trace)
{-

todo : define bytecode / assembly

1   PUSH    push a value on stack (next bc value is value address)
2   POP  pop a value from stack
3   ADD  add the two topmost values and push result 
4   SUB  substract the two topmost values and push result
5   MUL  multiply the two topmost values and push result
6   MUL  multiply the two topmost values and push result
7   EQ  push true if two top values are equals
8   GT  push true if n-1 > n
9   LT  push true if n-1 < n
10  JMP  move code pointer to next bytecode avlue 
11  JT   move code pointer to next bytecode avlue if top stack value is True (true == 1)
12  JNT      move code pointer to next bytecode avlue if top stack value is Not True  (False == 0)
13  PRT  print top most value     


-}





compileAst :: Stmt -> Machine -> Machine
compileAst stmt machine = case stmt of 
    Seq stmts -> Machine [0,1,2,3,4,5,6] [] [] []
    While cond stm -> Machine [6,7,8] [] [] []
    If cond thenStmt elseStmt -> Machine [9,10,11,12] [] [] []
    Print expr -> Machine [13] [] [] []
    Skip -> Machine [] [] [] []

compileAExpr :: AExpr ->  Machine -> Machine        
compileAExpr expr machine = case expr of
    IntConst i -> Machine (bytecode machine++[1,(toInteger (length( heap machine))) + 1]) (stack machine) (heap machine++[IntVal i])  (heapAddresses machine)
    Var n -> Machine (bytecode machine++[1,(getVariable n (heapAddresses machine))]) (stack machine) (heap machine) (addOrReplace n (toInteger (length (heap machine))) (heapAddresses machine))

compileBExpr :: BExpr ->  Machine -> Machine        
compileBExpr expr machine = case expr of
    BoolConst b -> Machine (bytecode machine++[1,(toInteger (length( heap machine))) + 1]) (stack machine) (heap machine++[BoolVal b]) (heapAddresses machine)
