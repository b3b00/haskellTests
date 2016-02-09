import System.Environment
import Data.List
import Debug.Trace
import GHC.Float
{-
**************************************
 RPN (Reverse Polish Notation) evaluator
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
	returns True if str represents an Operation,
			False if str is a number
			throws an error otherwise	
-}	  

isOp :: String -> Bool
isOp x =	case operation x  of
			Nothing -> False
			Just op -> True

isOperation :: String -> Bool
isOperation x = not $ isNumber x

{-
	Split an expression (represented as a string array) into a tuple containing
	    - a number stack (as string array)
		- a operation stack (as string array)
-}
splitExpression :: [String] -> ([String],[String])
splitExpression expr = ((filter isNumber expr), (filter (isOperation) expr))

convert :: ([String],[String]) -> ([Double],[String]) 
convert (n,o) = ((map (read::String->Double) n),o) 

{-
	parse a string into a double stack
		- a number stack (as double array)
		- an operation stack (as string array)
-}
parse :: String -> ([Double],[String])
parse code = convert (splitExpression (words code))


data OperationDescriptor = Operation_1 String (Double -> Double) | Operation_2 String (Double -> Double -> Double) | Operation_n String (Double -> Double -> Double) | No_Op String (Double -> Double)

display :: OperationDescriptor -> String
display (Operation_1 name f) = name++"/1"
display (Operation_2 name f) = name++"/1"
display (Operation_n name f) = name++"/n"
display (No_Op name f)  = "NOOP ("++name++")"

evaluateOperation :: OperationDescriptor -> [Double] -> [Double]
evaluateOperation (Operation_1 name func ) operands = [func $ head operands]++(tail operands)
evaluateOperation (Operation_2 name func ) operands = [func (operands!!0) (operands!!1)]++(drop 2 operands)
evaluateOperation (Operation_n name func ) operands = [foldl1 func operands]
evaluateOperation (No_Op name func) operands = error ("unknown operation :: "++name)




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
 
evalOp :: String -> [Double] -> [Double]
evalOp name nums = evaluateOperation (stackOp name) nums  
 


operation :: String -> Maybe (Double -> Double -> Double)
operation op 
   | op == "+" = Just (+)
   | op == "-" = Just (-)
   | op == "*" = Just (*)
   | op == "/" = Just (/)
   | otherwise = Nothing

   
   
 
{-
single operation evaluation
	- op : operation string
	- args : 2 Double array
-}	
evaluate :: String -> [Double] -> Double	
evaluate op args = case operation op  of
			Nothing -> error ("unknown operation : " ++ op)
			Just opFunc ->  ( opFunc (args!!0) (args!!1) )
			
{-
main loop : evaluate a double stack 
   - nums : Double stack
   - ops : operations stack
-}

run :: [Double] -> [String] -> Double
run nums ops 
	| null ops && (length nums > 1) = error "bad expression, unable to evaluate (missing or wrong operator ?)"
	| null ops = traceShow("no more op: ",nums) head nums
	| (length ops >= 1) && (length nums >= 1) =  run (evalOp (head ops) nums) (tail ops)  
	| otherwise = error "bad expression !"

computeRPN :: String -> Double
computeRPN expr = run n o 
		where (n,o) = parse expr
	
	
eval :: [Double] -> [String] -> Double
eval nums ops
	| null ops && (length nums > 1) = error "bad expression, unable to evaluate (missing or wrong operator ?)"
	| null ops = traceShow("no more op: ",nums) head nums
	| (length ops >= 1) && (length nums >= 2) = eval ([(evaluate (head ops) (take 2 nums))]++(drop 2 nums)) (tail ops)
	| otherwise = error "bad expression, unable to evaluate"

{-
	entry point : evaluate an expression 
	   - expr t: the expression as a string
-}
rpn :: String -> Double
rpn expr = eval n o
	where (n,o) = parse expr
	
{-
	******* MAIN *******

main = do
  expr <- getLine
  putStrLn (show (rpn expr))	
-}	
	


	  
		  
