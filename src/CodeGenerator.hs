module CodeGenerator where
import Data.Text
import Parser (Exp (..), Stm (..))
import qualified Data.Map as Map

data Instr = MOVE Temp Temp
           | MOVEI Temp Int
           | OP BinOP Temp Temp Temp
           | OPI BinOP Temp Temp Int -- TODO: check if this is redundant, if so, remove (as well as the ASM instructions)
           | LABEL Label
           | JUMP Label
           | COND BinOP Temp Temp Label Label
           | READLN
           | PRINT' Temp
           | RETURN' Temp
           | NEG Temp
           | NOT Temp
    deriving Show

-- ISSUE: Ambiguity between Parsers Exp And/Or labels and Cond BinOP And/Or
data State = State {table:: Map.Map String Int, registerCount :: Int, labelCount :: Int}
    deriving Show

type Prog = [Stm]
type Temp = String
type Id = String
type Label = String
type Table = [(String, Int)]
type Supply = (Int, Int)

initialState = State {table = Map.empty , registerCount = 0, labelCount = 0}
data BinOP = Sum | Sub | Mult | Divide | Modulus | Lt | Lteq | Eq | Neq | Gt | Gteq | AndC | OrC
    deriving Show

newTemp :: Supply -> (Temp, Supply)
newTemp (temps, labels) = ("t"++show temps, (temps +1, labels))

newTemp' :: State -> (Temp, State)
newTemp' s = ("t" ++ show temp, s {registerCount = temp +1})
    where temp = registerCount s

newLabel :: Supply -> (Label, Supply)
newLabel (temps, labels) = ("l"++show labels, (temps, labels+1))

newLabel' :: State -> (Label, State)
newLabel' s = ("l" ++ show label, s {labelCount = label+1})
    where label = registerCount s

popTemp :: Int -> Supply -> Supply
popTemp x (temps, labels) = (temps - x, labels)

popTemp' :: Int -> State -> State
popTemp' x s = s {registerCount = currCount - x}
    where currCount = registerCount s

-- TODO: check if there's a better way than to copy paste this stuff for every arithmetic expressions

transExp' :: Exp ->  Temp -> State -> ([Instr], State)
transExp' (Int n) dest state = ([MOVEI dest n], state)
transExp' Readln dest state = ([READLN], state)
transExp' (Plus e1 e2)  dest state
  = let (t1, state1) = newTemp' state
        (t2, state2) = newTemp' state1
        (code1, state3) = transExp' e1  t1 state2
        (code2, state4) = transExp' e2  t2 state3
        code = code1 ++ code2 ++ [OP Sum dest t1 t2]
    in (code, state4)
transExp' (Minus e1 e2)  dest state
  = let (t1, state1) = newTemp' state
        (t2, state2) = newTemp' state1
        (code1, state3) = transExp' e1  t1 state2
        (code2, state4) = transExp' e2  t2 state3
        code = code1 ++ code2 ++ [OP Sub dest t1 t2]
    in (code, state4)
transExp' (Times e1 e2)  dest state
  = let (t1, state1) = newTemp' state
        (t2, state2) = newTemp' state1
        (code1, state3) = transExp' e1  t1 state2
        (code2, state4) = transExp' e2  t2 state3
        code = code1 ++ code2 ++ [OP Mult dest t1 t2]
    in (code, state4)
transExp' (Div e1 e2)  dest state
  = let (t1, state1) = newTemp' state
        (t2, state2) = newTemp' state1
        (code1, state3) = transExp' e1  t1 state2
        (code2, state4) = transExp' e2  t2 state3
        code = code1 ++ code2 ++ [OP Divide dest t1 t2]
    in (code, state4)
transExp' (Mod e1 e2)  dest state
  = let (t1, state1) = newTemp' state
        (t2, state2) = newTemp' state1
        (code1, state3) = transExp' e1  t1 state2
        (code2, state4) = transExp' e2  t2 state3
        code = code1 ++ code2 ++ [OP Modulus dest t1 t2]
    in (code, state4)
transExp' (Negate e)  dest state
  = let (t1, state1) = newTemp' state
        (code1, state2) = transExp' e  t1 state1
        code = code1 ++ [NEG t1]
    in (code, state2)
transExp' (Not e)  dest state
  = let (t1, state1) = newTemp' state
        (code1, state2) = transExp' e  t1 state1
        code = code1 ++ [NOT t1]
    in (code, state2)
transExp' (SubExp e)  dest state = transExp' e dest state


-- TODO: Is it necessary to implement the relational OP (==, <, >, <=, >=) in transExp'if I already have them here?
-- TODO: necessary to consider that a condition may be a SUM, a SUB, a DIV, etc...?
transCond':: Exp -> Label -> Label -> State -> ([Instr], State)
transCond' e l1 l2 s = case e of
    Equal e1 e2 -> let (t1, state1) = newTemp' s
                       (t2, state2) = newTemp' state1
                       (code1, state3) = transExp' e1 t1 state2 
                       (code2, state4) = transExp' e2 t2 state3
                       code = code1 ++ code2 ++ [COND Eq t1 t2 l1 l2]
                    in (code, state4)
    Nequal e1 e2 -> let (t1, state1) = newTemp' s
                        (t2, state2) = newTemp' state1
                        (code1, state3) = transExp' e1 t1 state2 
                        (code2, state4) = transExp' e2 t2 state3
                        code = code1 ++ code2 ++ [COND Neq t1 t2 l1 l2]
                    in (code, state4)
    Greatereq e1 e2 -> let (t1, state1) = newTemp' s
                           (t2, state2) = newTemp' state1
                           (code1, state3) = transExp' e1 t1 state2 
                           (code2, state4) = transExp' e2 t2 state3
                           code = code1 ++ code2 ++ [COND Gteq t1 t2 l1 l2]
                        in (code, state4)
    Lesseq e1 e2 -> let (t1, state1) = newTemp' s
                        (t2, state2) = newTemp' state1
                        (code1, state3) = transExp' e1 t1 state2 
                        (code2, state4) = transExp' e2 t2 state3
                        code = code1 ++ code2 ++ [COND Lteq t1 t2 l1 l2]
                    in (code, state4)
    Greater e1 e2 -> let (t1, state1) = newTemp' s
                         (t2, state2) = newTemp' state1
                         (code1, state3) = transExp' e1 t1 state2 
                         (code2, state4) = transExp' e2 t2 state3
                         code = code1 ++ code2 ++ [COND Gt t1 t2 l1 l2]
                     in (code, state4)
    Less e1 e2 -> let (t1, state1) = newTemp' s
                      (t2, state2) = newTemp' state1
                      (code1, state3) = transExp' e1 t1 state2 
                      (code2, state4) = transExp' e2 t2 state3
                      code = code1 ++ code2 ++ [COND Lt t1 t2 l1 l2]
                  in (code, state4)
    And e1 e2 -> let (t1, state1) = newTemp' s
                     (t2, state2) = newTemp' state1
                     (code1, state3) = transExp' e1 t1 state2 
                     (code2, state4) = transExp' e2 t2 state3
                     code = code1 ++ code2 ++ [COND AndC t1 t2 l1 l2]
                 in (code, state4)
    Or e1 e2 -> let (t1, state1) = newTemp' s
                    (t2, state2) = newTemp' state1
                    (code1, state3) = transExp' e1 t1 state2 
                    (code2, state4) = transExp' e2 t2 state3
                    code = code1 ++ code2 ++ [COND OrC t1 t2 l1 l2]
                in (code, state4)
    Not e -> let (t1, state1) = newTemp' s
                 (t2, state2) = newTemp' state1
                 (code1, state3) = transExp' e t1 state2 
                 code = code1 ++ [COND AndC t1 t1 l1 l2]
             in (code, state3)
 

-- NOTE: Var Id Exp and Assign Id Exp might require a new new function, since they must return a new table (I believe)
-- TODO: The RETURN instruction, is it necessary? (since we dont have function calling)
transStm' :: Stm -> State -> ([Instr], State)
tranStm' EmptyStm s = ([], s)
transStm' stm s = case stm of
  (If e stm) -> let (l1, state1) = newLabel' s
                    (l2, state2) = newLabel' state1
                    (code1, state3) = transCond' e l1 l1 state2
                    (code2 , state4) = transStm' stm state3
                    code = code1 ++ [LABEL l1] ++ code2 ++ [LABEL l2]
                in (code, state4)
  (IfElse e stm1 stm2) -> let (l1, state1) = newLabel' s
                              (l2, state2) = newLabel' state1
                              (l3, state3) = newLabel' state2
                              (code1, state4) = transCond' e l1 l2 state3
                              (code2, state5) = transStm' stm1 state4
                              (code3, state6) = transStm' stm2 state5
                              code = code1 ++ [LABEL l1] ++ code2 ++ [JUMP l3] ++ [LABEL l2] ++ code3 ++ [LABEL l3]
                          in (code, state6)
  (While e stm) -> let (l1, state1) = newLabel' s
                       (l2, state2) = newLabel' state1
                       (l3, state3) = newLabel' state2
                       (code1, state4) = transCond' e l2 l3 state3
                       (code2, state5) = transStm' stm state4
                       code = [LABEL l1] ++ code1 ++ [LABEL l2] ++ code2 ++ [LABEL l1] ++ [LABEL l3]
                    in (code, state5)
  (Print e)  -> let (t1, state1) = newTemp' s
                    (code1, state2) = transExp' e t1 state1
                    code = code1 ++ [PRINT' t1]
                in (code, state2)
  (Return e) -> let (t1, state1) = newTemp' s
                    (code1, state2) = transExp' e t1 state1
                    code = code1 ++ [RETURN' t1]
                in (code, state2) 
-- TODO remove Expression from Return and find a way to quit progam early (Syscall or End Label)

{-
> Generate intermediary code:
Prog [0/2] (sequence of stms, and empty)
BlkORStm [0/2] (stm or block) Unecessary, I believe (simply place the label after the statements in the generated assembly)
Statements [5/7] (var, id)
Expressions [18/19] (id)
Release temporary/registers (function)
Implement table for variables (plus scoping, plus relevant information) (necessary?)

> Generate assembly
-}
