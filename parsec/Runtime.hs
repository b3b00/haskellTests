module Runtime where
import System.IO
import Control.Monad
import Ast
import Debug.Trace (trace)



data Value = StringV String
           | IntegerV Integer
           | BoolV Bool
           | NullV
            deriving (Show, Eq)



type EvalContext = [(String, Value)]


valueToString :: Value  -> String
valueToString sv = case sv of
    IntegerV x -> (show x)
    BoolV b -> (show b)
    StringV s -> s
    NullV -> "null"

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

play :: Stmt -> EvalContext -> IO()
play stmt evalContext = do    
    result <- evalStmt stmt []
    putStrLn (show result)

run :: Stmt -> IO()
run stmt = do
    play stmt []

    

{-
**** statements ****
-}        

getMessage :: Value -> String
getMessage msg = case msg of
            BoolV b -> (show b)  
            IntegerV i -> (show i) 
            StringV s -> (trace("GETMESSAGE "++ s)) s  
            NullV -> "NULL"

evalPrint ::     Value -> EvalContext -> IO EvalContext
evalPrint msg evalContext = 
    do 
        let str = (trace ("PRINTING "++(show msg))) getMessage msg  
        putStrLn ((show str))
        return evalContext

evalSeq :: [Stmt] -> EvalContext -> IO EvalContext
evalSeq stmts evalContext = case stmts of 
    [] -> do return evalContext
    (stm:tail) -> do
        newContext <- (evalStmt stm evalContext) 
        evalSeq tail newContext
        

evalWhile :: Expr -> Stmt -> EvalContext -> IO EvalContext
evalWhile cond stmt evalContext 
    | (evalExpr cond evalContext) == (BoolV False) = 
        do 
            return evalContext
    | otherwise = 
        do
            newContext <- (evalStmt stmt evalContext)
            evalWhile cond stmt newContext
    
    
evalIfThenElse :: Expr -> Stmt -> Stmt -> EvalContext -> IO EvalContext
evalIfThenElse cond stmThen stmElse evalContext
    | (evalExpr cond evalContext)==(BoolV True) = 
        do
            (evalStmt stmThen evalContext)
    | otherwise = 
        do 
            (evalStmt stmElse evalContext)
    
evalStmt :: Stmt -> EvalContext -> IO EvalContext
evalStmt stm evalContext = case stm of
    Assign pos var expr ->  do
        return (addOrReplaceValue var (evalExpr expr evalContext) evalContext)
    --AssignB var expr ->  addOrReplaceInteger var (evalExpr expr evalContext) evalContext
    If pos cond thenStmt elseStmt -> do 
        evalIfThenElse cond thenStmt elseStmt evalContext
    While pos cond stm -> do 
        evalWhile cond stm evalContext
    Seq stmts -> do 
        evalSeq stmts evalContext         
    Print pos expr -> do 
        evalPrint (evalExpr expr evalContext) evalContext
    Skip -> do
        return evalContext


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
    StringConst pos s -> StringV s
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
    Binary Concat pos left right -> evalStringOp evalContext (++) left right
    Var pos n -> getVariableVal n evalContext
    

evalBoolOp ::  EvalContext -> (Bool -> Bool -> Bool) -> Expr -> Expr -> Value 
evalBoolOp context  op  b1 b2  =  BoolV (op (getBoolValue (evalExpr b1 context)) (getBoolValue (evalExpr b2 context)))

evalIntegerOp ::  EvalContext -> (Integer -> Integer -> Integer) -> Expr -> Expr -> Value 
evalIntegerOp context op i1 i2  =  IntegerV (op (getIntegerValue (evalExpr i1 context)) (getIntegerValue (evalExpr i2 context)))

evalRelIntegerOp ::  EvalContext -> (Integer -> Integer -> Bool) -> Expr -> Expr -> Value 
evalRelIntegerOp context op i1 i2  =  BoolV (op (getIntegerValue (evalExpr i1 context)) (getIntegerValue (evalExpr i2 context)))

{-evalStringOp :: EvalContext ->  (String -> String -> String) -> Expr -> Expr ->  Value
evalStringOp context op s1 s2 = let val1 = (valueToString (evalExpr s1 context)) in
                                    let val2 = (trace ("C1 :: "++val2)) (valueToString (evalExpr s2 context)) in
                                        let  concat = (trace ("C2 :: "++val2))  (op val1 val2) in                
                                            (trace ("C3 :: "++concat)) StringV (concat)-}

evalStringOp :: EvalContext ->  (String -> String -> String) -> Expr -> Expr ->  Value
evalStringOp context op s1 s2 = StringV (op (valueToString (evalExpr s1 context)) (valueToString (evalExpr s2 context)))           


