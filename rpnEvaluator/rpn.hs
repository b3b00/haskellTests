module RPN where

import System.Environment
import Data.List
import Debug.Trace
import GHC.Float
{-
******************************************************************************************************************
                            RPN (Reverse Polish Notation) evaluator
******************************************************************************************************************
-}

{-
**************************************
operation Type
**************************************    
-}

data OperationDescriptor = Operation_1 String (Double -> Double) | Operation_2 String (Double -> Double -> Double) | Operation_n String (Double -> Double -> Double) | No_Op String (Double -> Double)


stackOp :: String -> OperationDescriptor 
stackOp op 
   | op == "+" = Operation_2 "+" (+)
   | op == "-" = Operation_2 "-" (-)
   | op == "*" = Operation_2 "*" (*)
   | op == "/" = Operation_2 "/" (/)
   | op == "sin" = Operation_1 "sin" (sin)
   | op == "P" = Operation_n "P" (*)
   | op == "S" = Operation_n "S" (+)
   | otherwise = No_Op op (\x -> x)


-- single operation evaluation

evaluateOperation :: OperationDescriptor -> [Double] -> [Double]
evaluateOperation (Operation_1 name func ) operands = [func $ head operands]++(tail operands)
evaluateOperation (Operation_2 name func ) operands = [func (operands!!0) (operands!!1)]++(drop 2 operands)
evaluateOperation (Operation_n name func ) operands = [foldl1 func operands]
evaluateOperation (No_Op name func) operands = error ("unknown operation :: "++name)

evalOp :: OperationDescriptor -> [Double] -> [Double]
evalOp operation nums = evaluateOperation operation nums  

{-
**************************************
RPN parsing
**************************************    
-}


{-
    returns True if str represents a number
-}
isNumber :: String -> Bool
isNumber str =
    case (reads str) :: [(Double, String)] of
      [(_, "")] -> True
      _ -> False


{-
    returns True if x represents an Operation,
            False if x is a number
                        
-}
isOperation :: String -> Bool
isOperation x = not $ isNumber x 

{-
    Split an expression (represented as a string array) into a tuple containing
        - a number stack (as string array)
        - a operation stack (as string array)
-}
splitExpression :: [String] -> ([String],[String])
splitExpression expr = ((filter isNumber expr), (filter isOperation expr))

-- convert parsing result from ([String],[String]) to ([OperationDescriptor], [Double])
-- ready to be evaluated
convert :: ([String],[String]) -> ([Double],[OperationDescriptor]) 
convert (n,o) = ((map (read::String->Double) n),(map stackOp o)) 

{-
    parse a string into a double stack
        - a number stack (as double array)
        - an operation stack (as string array)
-}
parse :: String -> ([Double],[OperationDescriptor])
parse code = convert (splitExpression (words code))
 

 

{-
**************************************
main loop : evaluate a double stack 
   - nums : Double stack
   - ops : operations stack
**************************************   
-}

run :: [Double] -> [OperationDescriptor] -> Double
run nums ops 
    | null ops && (length nums > 1) = error "bad expression, unable to evaluate (missing or wrong operator ?)"
    | null ops =  head nums
    | (length ops >= 1) && (length nums >= 1) =  run (evalOp (head ops) nums) (tail ops)  
    | otherwise = error "bad expression !"

    
{-
    entry point : evaluate an expression 
       - expr t: the expression as a string
-}    
computeRPN :: String -> Double
computeRPN expr = run n o 
        where (n,o) = parse expr
    
    
    


      
          
