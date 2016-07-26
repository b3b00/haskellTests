-- | virtual machine for parsec test
module Machine where 
import System.IO
import Control.Monad
import Ast
import Debug.Trace (trace)
import Stack


{- |
 

1.   PUSH push a value on stack (next bc value is value address)
2.   POP pop a value from stack
3.   MOV pop value from stack and store in heap at next bytecode
    
10.  JMP move code pointer to next bytecode avlue
11.  JT  move code pointer to next bytecode avlue if top stack value is True
12.  JNT move code pointer to next bytecode avlue if top stack value is False
  
20.  PRINT print top most value
21.  NOOP no operation

    
50.  ADD add the two topmost values and push result
51.  SUB substract the two topmost values and push result
52.  MUL multiply the two topmost values and push result
53.  DIV divide the two topmost values and push result
54.  NEG negate top most value on stack
55.  NOT push true if top most is false, false otherwise
56.  AND push and value of 2 topmost
57.  OR push or value of 2 topmost
58.  EQ push true if two top values are equals
59.  GT push true if n-1 > n
60.  LT push true if n-1 < n
61.  CONCAT contenate 2 top most value and push result


-}

{- 
une machine est consitutée de 
 - un bytecode [Integer]
 - une pile de valeur [StackValue]
 - un tas [StackValue] pour les constantes
-}

{- | une machine est la structure d'une VM
 elle est constitutée de 

    * un pointeur d'instruction (l'index de l'instruction courante dans le bytecode)
    * une liste d'opcode
    * une pile de valeur utilisée à l'exécution
    * une zone mémoire (tas) pour  

        *les valeurs constantes
        *les valeurs de variables

    * la table des addresses de variables dans le tas
-}  

stackOpsStart:: Int
stackOpsStart = 1
stackOpEnd:: Int
stackOpEnd = 3

jumpOpStart:: Int
jumpOpStart = 10
jummOpEnd:: Int
jummOpEnd = 12

otherOpStart:: Int
otherOpStart = 20
otherOpEnd:: Int
otherOpEnd = 21

binaryOpStart:: Int
binaryOpStart = 50
binaryOPEnd:: Int
binaryOPEnd = 61

opcodesToMemo :: [(Int,String)]
opcodesToMemo = [(1,"PUSH")
                ,(2,"POP")
                ,(3,"MOV")
                ,(10,"JMP")
                ,(11,"JT")
                ,(12,"JNT")
                ,(20,"PRINT")
                ,(21,"NOOP")
                ,(50,"ADD")
                ,(51,"SUB")
                ,(52,"MUL")
                ,(53,"DIV")
                ,(54,"NEG")
                ,(55,"NOT")
                ,(56,"AND")
                ,(57,"OR")
                ,(58,"EQ")
                ,(59,"GT")
                ,(60,"LT")
                ,(61,"CONCAT")
                ]

memoToOpcode :: [(String,Int)]
memoToOpcode = [(val,key) |  (key, val) <- opcodesToMemo]




data Machine = Machine {
    pointer :: Int
    , bytecode :: [Int]
    , stack :: [StackValue]
    , heap :: [StackValue]
    , heapAddresses :: [(String,Int)]
} deriving (Show, Eq, Read)


-- | retourne un memocode d'instruction (String) à partir d'un opcode
memCode :: Int -> String
memCode bc = case lookup bc opcodesToMemo of
    Just m -> m
    Nothing -> ("UNKNOWN ["++(show bc)++"]")

-- | liste des instructions (opcodes) ne prenant pas de paramètres
zeroParamsOpCodes = [2,20,21]++[50..]

-- | retourne l'opcode de l'instruction courante de la machine
opCode :: Machine -> Int
opCode machine = (bytecode machine) !! (pointer machine)

-- retourne l'opcode situé à l'offset donné par rapport à l'instruction courante
opCodeRel :: Machine -> Int -> Int
opCodeRel machine offset = 
    let opcodeOffset =(pointer machine)+offset in
        let  op =    ((bytecode machine) !! opcodeOffset) in
            op 

    --(bytecode machine) !! ((pointer machine)+offset)

-- | retourne True si l'opcode de l'instruction courante est dans la liste en paramètre
opCodeIn :: Machine -> [Int] -> Bool 
opCodeIn machine codes = elem (opCode machine) codes


-- | retourne l'adresse dans le tas de la variable passée en paramètre
getVariableAddress :: String -> Machine -> Int
getVariableAddress name machine = case lookup name (heapAddresses machine) of
    Just n -> n
    Nothing -> -1


-- | ajoute la variable dans le tas 
-- si la  variable existe déjà ne crée pas de nouvelle entrée dans le tas
setAddressForVariableInHeap :: String -> Machine -> Machine
setAddressForVariableInHeap name machine = case lookup name (heapAddresses machine) of 
    Just n ->  machine
    Nothing -> let address = length (heap machine) in
         (Machine 0 (bytecode machine) (stack machine) ((heap machine)++[NullVal]) ((heapAddresses machine)++[(name,address)]))

-- | retourne un dump du tas pour les variables sous forme de chaine
-- à partir d'un la table d'allocation des variables et d'un tas
dump ::  [(String,Int)] -> [StackValue] -> String
dump addresses heap = case addresses of
    [] -> ""
    ((name,addr):addrs) -> (name++"::"++(show (heap !! addr))++"\n")++(dump addrs heap)

-- | retourne un dump du tas pour les variables sous forme de chaine
dumpHeap :: Machine -> String        
dumpHeap machine = dump (heapAddresses machine) (heap machine)

-- | retourne une représentation du bytecode sous forme d'un programme de memocode
printAssembly :: Machine -> String
printAssembly machine  = printAssemblyBC (bytecode machine)

printAssemblyOp0 :: Int -> [Int] -> (String, [Int])  
printAssemblyOp0 op bc = ((memCode op),(drop 1 bc))

printAssemblyOp1 :: Int -> [Int] -> (String, [Int])  
printAssemblyOp1 op bc = ( ((memCode op)++" "++(show (bc!!1))) ,(drop 2 bc))


printAssemblyOp :: Int -> [Int] -> (String,[Int])
printAssemblyOp op bc 
    | (elem op zeroParamsOpCodes) = printAssemblyOp0 op bc
    | otherwise = printAssemblyOp1 op bc


printAssemblyBC :: [Int] -> String  
printAssemblyBC bc = case bc of 
    [] -> ""
    (x:xs) ->  let r = (printAssemblyOp x bc) in
                        (fst r)++"\n"++(printAssemblyBC (snd r))

-- | retourne la valeur d'une variable               
getHeapValue:: String -> Machine -> StackValue
getHeapValue  name machine = (heap machine) !! (getVariableAddress name machine)