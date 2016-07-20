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
  | UnknownExpr
  | ErrorExpr
  deriving (Eq)

instance Show ExprType where
  show expr = case expr of 
    IntExpr -> "int"
    BoolExpr -> "bool"
    UnknownExpr -> "unknwon"
    ErrorExpr -> "error"


type  CheckResult = (ExprType, [String])  

type SemanticContext = ([(String, ExprType)],[String])


-- | vérifie la correction sémantique
-- retourne  une liste d'erreur. Si le programme est correct retourne une liste vide.
-- check :: Stmt -> SemanticContext
semanticCheck :: Stmt ->[String]
semanticCheck stmt = snd (check stmt  ([],[]) )


third (_, _, x) = x
second(_,x,_) = x
first(_,x,_) = x


showPos ::  SourcePos -> String
showPos pos = (show (sourceLine pos))++", "++(show (sourceColumn pos))


{- ****************************
    statements
-}




checkAssign ::  Stmt -> SemanticContext -> SemanticContext
checkAssign assign context = case assign of
    Assign pos varName expr -> let assignCheck = getExprType expr (fst context) in
                                let varType = getVariableType varName (fst context) in
                                    if varType == (fst assignCheck) then
                                        ( (addOrReplaceVarType varName  varType (fst context)) ,[]) -- first assignement for variable or further correct assignement
                                    else 
                                        (fst context, (snd context)++["bad assignement at "++(showPos pos)++" : expeecting "++(show varType)++" but found "++(show (fst assignCheck))])
    otherwise -> error "bas execution path : assign check"     


CheckSequence :: [Stmt] ->  SemanticContext -> SemanticContext
compileSequence [] context = context
compileSequence (h:t) context = compileSequence t (compileStmt h context)      

check :: Stmt -> SemanticContext -> SemanticContext
check stmt context = case stmt of
    Skip -> ([],[])
    Print pos expr -> let prtCheck = getExprType expr (fst context) in
        ((fst context),(snd context)++(snd prtCheck))
    Assign pos var expr -> checkAssign stmt context   
    Seq stmts -> 
    otherwise -> ((fst context),["TODO : "++(show stmt)])






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
    otherwise -> (ErrorExpr,["uncompatible types ("++(show (second types))++" and "++(show (third types))++")for "++(show (first types))++" at "++(showPos pos)])


{-getBinaryExprType :: BinOp -> SourcePos -> Expr -> Expr -> [(String,ExprType)]-> CheckResult
getBinaryExprType op pos left right context =
    case op of
        Add -> let rt = fst (getExprType right) in
                let lt = fst (getExprType left) in
                    binaryCompatibilty (op,rt,lt) pos-}



getExprType :: Expr -> [(String,ExprType)] -> CheckResult
getExprType expr varTypes = case expr of 
    IntConst i pos ->  (IntExpr,[])
    BoolConst b pos -> (BoolExpr,[])
    Var pos n -> let varType = (getVariableType n varTypes) in
                    if varType == UnknownExpr then
                         (varType, ["unknwon variable "++n++" at "++(showPos pos)])
                    else (varType,[])
    Binary op pos left right -> let lt = getExprType left varTypes in
                                    let rt = getExprType right varTypes in
                                        let compat = binaryCompatibilty (op,fst lt,fst lt) pos in
                                            (fst compat, (snd compat)++(snd lt)++(snd rt))                                            
    Neg pos exprN ->  let rightType = (trace ("testing - "++(show exprN))) getExprType exprN varTypes in
        if (fst rightType) == IntExpr then (IntExpr,[]) else (ErrorExpr ,["bad type for unary - operator at "++(showPos pos)]++(snd rightType))       
    Not pos exprN -> let rightType = (trace ("testing not "++(show exprN))) getExprType exprN varTypes in
        if ((fst rightType) == BoolExpr) then (BoolExpr,[]) else (ErrorExpr ,["bad type for unary 'not' operator at "++(showPos pos)]++(snd rightType)) 
            




