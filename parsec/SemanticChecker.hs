module SemanticChecker where 
import System.IO
import Control.Monad
import Ast
import Machine
import Stack
import Assoc
import Debug.Trace (trace)
import Text.Parsec(SourcePos)
import Text.Parsec.Pos
import qualified Data.Map as Map  


getVariableType :: String -> [(String,ExprType)] -> ExprType
getVariableType varName types = case lookup varName types of
  Just n  -> n
  Nothing -> UnknownExpr

addOrReplaceVarType :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
addOrReplaceVarType key value assoc = (key,value):(filter ((key /=).fst) assoc)  



data ExprType = IntExpr
  | BoolExpr
  | StringExpr
  | UnknownExpr
  | ErrorExpr
  deriving (Eq)

instance Show ExprType where
  show expr = case expr of 
    IntExpr -> "int"
    BoolExpr -> "bool"
    StringExpr -> "string"
    UnknownExpr -> "unknwon"
    ErrorExpr -> "error"


type  CheckResult = (ExprType, [String])  

type SemanticContext = ([(String, ExprType)],[String])


-- | vérifie la correction sémantique
-- retourne  une liste d'erreur. Si le programme est correct retourne une liste vide.
-- check :: Stmt -> SemanticContext
semanticCheck :: Stmt ->[String]
semanticCheck stmt = snd (checkStatement stmt  ([],[]) )


third (_, _, x) = x
second(_,x,_) = x
first(x,_,_) = x


showPos ::  SourcePos -> String
showPos pos = (show (sourceLine pos))++", "++(show (sourceColumn pos))


{- ****************************
    statements
-}





checkAssign ::  Stmt -> SemanticContext -> SemanticContext
checkAssign assign context = case assign of
    Assign pos varName expr -> let assignCheck =  getExprType expr (fst context) in
                                let exprtype = (fst assignCheck) in    
                                    let varType =  getVariableType varName (fst context) in
                                        if varType == exprtype then
                                             context   
                                             -- correct assignement
                                        else 
                                            if varType == UnknownExpr then
                                                ( (addOrReplaceVarType varName  (fst assignCheck) (fst context)) ,[]) -- first assignement
                                            else
                                                if  exprtype == ErrorExpr then -- error in expression
                                                    (fst context, snd assignCheck)
                                                else  -- error in assignement
                                                    trace("error : "++(show (varType))) (fst context, (snd context)++["bad assignement at "++(showPos pos)++" : expecting "++(show varType)++" but found "++(show (fst assignCheck))]++(snd assignCheck))
    otherwise -> error "bas execution path : assign check"     


checkSequence :: [Stmt] ->  SemanticContext -> SemanticContext
checkSequence [] context = context
checkSequence (h:t) context =  checkSequence t (checkStatement h context)      

checkWhile :: Stmt -> SemanticContext -> SemanticContext
checkWhile stmt context = case stmt of
    While pos cond stmt -> let condtype = getExprType cond (fst context) in
                            if (fst condtype) == BoolExpr then
                                checkStatement stmt context
                            else
                                let stmtcheck = checkStatement stmt context in
                                    (fst stmtcheck, (snd stmtcheck)++[" error at "++(showPos pos)++" :  while condition expression is not a bool ("++(show (fst condtype))++")"])
    otherwise -> error ("bad execution path "++(show stmt))

checkIf :: Stmt -> SemanticContext -> SemanticContext
checkIf stmt context = case stmt of
    If pos cond ifStmt elseStmt  -> let condtype = getExprType cond (fst context) in
                                        if (fst condtype) == BoolExpr then
                                                let ifCheck = checkStatement ifStmt context in
                                                    checkStatement elseStmt ifCheck
                                        else
                                        let ifCheck = checkStatement ifStmt context in
                                            let elseCheck = checkStatement elseStmt ifCheck in
                                                (fst elseCheck, (snd elseCheck)++[" error at "++(showPos pos)++" : if condition expression is not a bool ("++(show (fst condtype))++")"])
    otherwise -> error "bad execution path"    

checkStatement :: Stmt -> SemanticContext -> SemanticContext
checkStatement stmt context = case stmt of
    Skip -> context
    Print pos expr -> let prtCheck = getExprType expr (fst context) in
        ((fst context),(snd context)++(snd prtCheck))
    Assign pos var expr -> checkAssign stmt context   
    Seq stmts -> checkSequence stmts context
    While pos cond loop -> checkWhile stmt context
    If pos cond thenStmt elseStmt -> checkIf stmt context






{- **********************
  expressions
-}

binaryCompatibilty :: (BinOp, ExprType, ExprType) -> SourcePos -> CheckResult
binaryCompatibilty types pos = case types  of
    (Add, IntExpr, IntExpr) -> (IntExpr,[])
    (Substract, IntExpr, IntExpr) -> (IntExpr,[])
    (Multiply, IntExpr, IntExpr) -> (IntExpr,[])
    (Divide, IntExpr, IntExpr) -> (IntExpr,[])
    (And, BoolExpr, BoolExpr) -> (BoolExpr,[])
    (Or, BoolExpr, BoolExpr) -> (BoolExpr,[])
    (Lesser, IntExpr, IntExpr) -> (BoolExpr,[])
    (Greater, IntExpr, IntExpr) -> (BoolExpr,[])
    (Equals, IntExpr, IntExpr) -> (BoolExpr,[])
    (Concat,StringExpr,StringExpr) -> (StringExpr,[])
    (Concat,StringExpr,IntExpr) -> (StringExpr,[])
    (Concat,StringExpr,BoolExpr) -> (StringExpr,[])
    (Concat,IntExpr,StringExpr) -> (StringExpr,[])
    (Concat,BoolExpr,StringExpr) -> (StringExpr,[])
    otherwise -> (ErrorExpr,["uncompatible types ("++(show (second types))++" and "++(show (third types))++") for "++(show (first types))++" at "++(showPos pos)])


getExprType :: Expr -> [(String,ExprType)] -> CheckResult
getExprType expr varTypes = case expr of 
    IntConst i pos ->  (IntExpr,[])
    BoolConst b pos -> (BoolExpr,[])
    StringConst str pos -> (StringExpr,[])
    Var pos n -> let varType = (getVariableType n varTypes) in
                    if varType == UnknownExpr then
                         (varType, ["unknwon variable "++n++" at "++(showPos pos)])
                    else (varType,[])
    Binary op pos left right -> let lt = getExprType left varTypes in
                                    let rt = getExprType right varTypes in
                                        let compat =  binaryCompatibilty (op,fst lt,fst rt) pos in
                                            (fst compat, (snd compat)++(snd lt)++(snd rt))                                            
    Neg pos exprN ->  let rightType = (trace ("testing - "++(show exprN))) getExprType exprN varTypes in
        if (fst rightType) == IntExpr then (IntExpr,[]) else (ErrorExpr ,["bad type for unary - operator at "++(showPos pos)]++(snd rightType))       
    Not pos exprN -> let rightType = (trace ("testing not "++(show exprN))) getExprType exprN varTypes in
        if ((fst rightType) == BoolExpr) then (BoolExpr,[]) else (ErrorExpr ,["bad type for unary 'not' operator at "++(showPos pos)]++(snd rightType)) 
            





