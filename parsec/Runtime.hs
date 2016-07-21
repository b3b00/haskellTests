module Runtime where
import System.IO
import Control.Monad
import Ast
import Debug.Trace (trace)



data Value = StrV String
           | IntegerV Integer
           | BoolV Bool
           | NullV
            deriving (Show, Eq)

type EvalContext = [(String, Value)]

{-
**** accesing variable mapping
-}
getVariableVal :: String -> EvalContext -> Value
getVariableVal name evalContext = case lookup name evalContext of
  Just n  -> n
  Nothing -> error ("unknwon variable "++name)

addOrReplaceValue :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
addOrReplaceValue key value assoc = (key,value):(filter ((key /=).fst) assoc)              

{-
**** entrypoint 
-}

--play :: Stmt -> List -> IO()
play stmt evalContext = let    
    result = evalStmt stmt []
    in
    putStrLn (show result)

run :: Stmt -> IO()
run stmt = do
    play stmt []

    

{-
**** statements ****
-}        

evalPrint ::     Value -> EvalContext -> EvalContext
evalPrint msg evalContext =  case msg of
    BoolV b -> trace ("PR    Value :: "++show b)  evalContext
    IntegerV i -> trace ("PR    Value :: "++show i)  evalContext
    StrV s -> trace ("PR    Value :: "++s)  evalContext
    NullV -> trace ("PR    Value :: NULL")  evalContext

evalSeq :: [Stmt] -> EvalContext -> EvalContext
evalSeq stmts evalContext = case stmts of 
    [] -> evalContext
    (stm:tail) -> evalSeq tail (evalStmt stm evalContext)

evalWhile :: Expr -> Stmt -> EvalContext -> EvalContext
evalWhile cond stmt evalContext 
    | (evalExpr cond evalContext) == (BoolV False) = evalContext
    | otherwise = evalWhile cond stmt (evalStmt stmt evalContext)
    
    
evalIfThenElse :: Expr -> Stmt -> Stmt -> EvalContext -> EvalContext
evalIfThenElse cond stmThen stmElse evalContext
    | (evalExpr cond evalContext)==(BoolV True) = (evalStmt stmThen evalContext)
    | otherwise = (evalStmt stmElse evalContext)
    
evalStmt :: Stmt -> EvalContext -> EvalContext
evalStmt stm evalContext = case stm of
    Assign pos var expr ->  addOrReplaceValue var (evalExpr expr evalContext) evalContext
    --AssignB var expr ->  addOrReplaceInteger var (evalExpr expr evalContext) evalContext
    If pos cond thenStmt elseStmt -> evalIfThenElse cond thenStmt elseStmt evalContext
    While pos cond stm -> evalWhile cond stm evalContext
    Seq stmts -> evalSeq stmts evalContext         
    Print pos expr -> evalPrint (evalExpr expr evalContext) evalContext
    Skip -> evalContext


{-
**** expressions evaluation
-}

getBoolValue :: Value -> Bool
getBoolValue  v = case v of 
    BoolV b -> b
    otherwise -> (error ("bad value "++(show v)++" (expecting bool)"))

getIntegerValue :: Value -> Integer
getIntegerValue v = case v of
    IntegerV i -> i
    otherwise -> error ("bad value "++(show v)++" (expecting integer)")


evalExpr :: Expr -> EvalContext -> Value
evalExpr expr evalContext = case expr of 
    BoolConst pos b -> BoolV b
    IntConst pos i -> IntegerV i
    Not pos e ->  BoolV (not (getBoolValue (evalExpr e evalContext)))
    Neg pos e ->  IntegerV (-(getIntegerValue (evalExpr e evalContext)))
    Binary And pos left right -> evalBoolOp evalContext (&&) left right
    Binary Or pos left right -> evalBoolOp evalContext (||) left right
    Binary Add pos left right -> evalIntegerOp evalContext (+) left right
    Binary Substract pos left right -> evalIntegerOp evalContext (-) left right
    Binary Multiply pos left right -> evalIntegerOp evalContext (*) left right
    Binary Divide pos left right -> evalIntegerOp evalContext (quot) left right
    Binary Lesser pos left right -> evalRelIntegerOp evalContext (<) left right
    Binary Greater pos left right -> evalRelIntegerOp evalContext (>) left right
    Binary Equals pos left right -> evalRelIntegerOp evalContext (==) left right
    Var pos n -> getVariableVal n evalContext
    

evalBoolOp ::  EvalContext -> (Bool -> Bool -> Bool) -> Expr -> Expr -> Value 
evalBoolOp context  op  b1 b2  =  BoolV (op (getBoolValue (evalExpr b1 context)) (getBoolValue (evalExpr b2 context)))

evalIntegerOp ::  EvalContext -> (Integer -> Integer -> Integer) -> Expr -> Expr -> Value 
evalIntegerOp context op i1 i2  =  IntegerV (op (getIntegerValue (evalExpr i1 context)) (getIntegerValue (evalExpr i2 context)))

evalRelIntegerOp ::  EvalContext -> (Integer -> Integer -> Bool) -> Expr -> Expr -> Value 
evalRelIntegerOp context op i1 i2  =  BoolV (op (getIntegerValue (evalExpr i1 context)) (getIntegerValue (evalExpr i2 context)))


