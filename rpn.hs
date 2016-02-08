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
-}
main = do
  expr <- getLine
  putStrLn (show (rpn expr))	
	
	


	  
		  
