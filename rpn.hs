import System.Environment
import Data.List
import Debug.Trace
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
isOperation :: String -> Bool
isOperation op 
    | elem op ["+","-","*","/"] = True
    | isNumber op = False
    | otherwise = error(op ++ " :: bad operator") False
--isOperation op = str == "+" || str == "-" || str == "*" || str == "/"
--isOperation op =  



{-
	Split an expression (represented as a string array) into a tuple containing
	    - a number stack (as string array)
		- a operation stack (as string array)
-}
splitExpression :: [String] -> ([String],[String])
splitExpression expr = ((filter isNumber expr), (filter isOperation expr))



convert :: ([String],[String]) -> ([Float],[String]) 
convert (n,o) = ((map (read::String->Float) n),o) 

{-
	parse a string into a double stack
		- a number stack (as double array)
		- an operation stack (as string array)
-}
parse :: String -> ([Float],[String])
parse code = convert (splitExpression (words code))


{-
single operation evaluation
	- op : operation string
	- args : 2 float array
-}	
evaluate :: String -> [Float] -> Float	
evaluate op args
   | op == "+" = traceShow ("+ "++(show args)) args!!0 + args!!1
   | op == "-" = traceShow ("- "++(show args)) args!!0 - args!!1
   | op == "*" = traceShow ("* "++(show args)) args!!0 * args!!1
   | op == "/" = traceShow ("/ "++(show args)) args!!0 / args!!1
   | otherwise = error ("unknown operation : " ++ op)


{-
main loop : evaluate a double stack 
   - nums : Float stack
   - ops : operations stack
-}
eval :: [Float] -> [String] -> Float
eval nums ops
	| null ops && (length nums > 1) = error "bad expression, unable to evaluate (missing operator ?)"
	| null ops = traceShow("no more op: ",nums) head nums
	| (length ops >= 1) && (length nums >= 2) = eval ([(evaluate (head ops) (take 2 nums))]++(drop 2 nums)) (tail ops)
	| otherwise = error "bad expression, unable to evaluate"

{-
	entry point : evaluate an expression 
	   - expr t: the expression as a string
-}
rpn :: String -> Float
rpn expr = eval n o
	where (n,o) = parse expr
	

  

   
main = do
	expr <- getLine
	putStrLn (show (rpn expr))	
	


	  
		  
