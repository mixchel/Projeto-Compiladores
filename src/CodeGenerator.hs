module CodeGenerator where
import Data.Text
import Parser (Exp (..), Stm (..))

data Instr = MOVE Temp Temp
           | MOVEI Temp Int
           | OP BinOP Temp Temp Temp
           | OPI BinOP Temp Temp Int
           | LABEL Label
           | JUMP Label
           | COND BinOP Temp Temp Label Label
           | READLN
           | PRINT' Temp
           | RETURN' Temp
           | NEG Temp
    deriving Show

-- ISSUE: Ambiguity between Parsers Exp And/Or labels and Cond BinOP And/Or
data BinOP = Sum | Sub | Mult | Divide | Modulus | Lt | Lteq | Eq | Neq | Gt | Gteq | AndC | OrC
    deriving Show
type Prog = [Stm]
type Temp = String
type Id = String
type Label = String
type Table = [(String, Int)]
type Supply = (Int, Int)

newTemp :: Supply -> (Temp, Supply)
newTemp (temps, labels) = ("t"++show temps, (temps +1, labels))

newLabel :: Supply -> (Label, Supply)
newLabel (temps, labels) = ("l"++show labels, (temps, labels+1))

popTemp :: Int -> Supply -> Supply
popTemp x (temps, labels) = (temps - x, labels)

-- TODO: subexp, identifier
-- TODO: check if there's a better way than to copy paste this stuff for every arithmetic expressions
transExp :: Exp -> Table -> Temp -> Supply -> ([Instr], Supply)
transExp (Int n) table dest supply = ([MOVEI dest n], supply)
transExp Readln table dest supply = ([READLN], supply)
transExp (Plus e1 e2) table dest supply
  = let (t1, supply1) = newTemp supply
        (t2, supply2) = newTemp supply1
        (code1, supply3) = transExp e1 table t1 supply2
        (code2, supply4) = transExp e2 table t2 supply3
        code = code1 ++ code2 ++ [OP Sum dest t1 t2]
    in (code, supply4)
transExp (Minus e1 e2) table dest supply
  = let (t1, supply1) = newTemp supply
        (t2, supply2) = newTemp supply1
        (code1, supply3) = transExp e1 table t1 supply2
        (code2, supply4) = transExp e2 table t2 supply3
        code = code1 ++ code2 ++ [OP Sub dest t1 t2]
    in (code, supply4)
transExp (Times e1 e2) table dest supply
  = let (t1, supply1) = newTemp supply
        (t2, supply2) = newTemp supply1
        (code1, supply3) = transExp e1 table t1 supply2
        (code2, supply4) = transExp e2 table t2 supply3
        code = code1 ++ code2 ++ [OP Mult dest t1 t2]
    in (code, supply4)
transExp (Div e1 e2) table dest supply
  = let (t1, supply1) = newTemp supply
        (t2, supply2) = newTemp supply1
        (code1, supply3) = transExp e1 table t1 supply2
        (code2, supply4) = transExp e2 table t2 supply3
        code = code1 ++ code2 ++ [OP Divide dest t1 t2]
    in (code, supply4)
transExp (Mod e1 e2) table dest supply
  = let (t1, supply1) = newTemp supply
        (t2, supply2) = newTemp supply1
        (code1, supply3) = transExp e1 table t1 supply2
        (code2, supply4) = transExp e2 table t2 supply3
        code = code1 ++ code2 ++ [OP Modulus dest t1 t2]
    in (code, supply4)
transExp (Negate e) table dest supply
  = let (t1, supply1) = newTemp supply
        (code1, supply2) = transExp e table t1 supply1
        code = code1 ++ [NEG t1]
    in (code, supply2)

-- TODO: Is it necessary to implement the relational OP (==, <, >, <=, >=) in transExp if I already have them here?
-- TODO: necessary to consider that a condition may be a SUM, a SUB, a DIV, etc...?
transCond :: Exp -> Table -> Label -> Label -> Supply -> ([Instr], Supply)
transCond e table l1 l2 supply = case e of
                     Equal e1 e2 -> let (t1, supply1) = newTemp supply
                                        (t2, supply2) = newTemp supply1
                                        (code1, supply3) = transExp e1 table t1 supply2
                                        (code2, supply4) = transExp e2 table t2 supply3
                                        code = code1 ++ code2 ++ [COND Eq t1 t2 l1 l2]
                                     in (code, supply4)
                     Nequal e1 e2 -> let (t1, supply1) = newTemp supply
                                         (t2, supply2) = newTemp supply1
                                         (code1, supply3) = transExp e1 table t1 supply2
                                         (code2, supply4) = transExp e2 table t2 supply3
                                         code = code1 ++ code2 ++ [COND Neq t1 t2 l1 l2]
                                      in (code, supply4)
                     Greatereq e1 e2 -> let (t1, supply1) = newTemp supply
                                            (t2, supply2) = newTemp supply1
                                            (code1, supply3) = transExp e1 table t1 supply2
                                            (code2, supply4) = transExp e2 table t2 supply3
                                            code = code1 ++ code2 ++ [COND Gteq t1 t2 l1 l2]
                                        in (code, supply4)
                     Lesseq e1 e2 -> let (t1, supply1) = newTemp supply
                                         (t2, supply2) = newTemp supply1
                                         (code1, supply3) = transExp e1 table t1 supply2
                                         (code2, supply4) = transExp e2 table t2 supply3
                                         code = code1 ++ code2 ++ [COND Lteq t1 t2 l1 l2]
                                      in (code, supply4)
                     Greater e1 e2 -> let (t1, supply1) = newTemp supply
                                          (t2, supply2) = newTemp supply1
                                          (code1, supply3) = transExp e1 table t1 supply2
                                          (code2, supply4) = transExp e2 table t2 supply3
                                          code = code1 ++ code2 ++ [COND Gt t1 t2 l1 l2]
                                       in (code, supply4)
                     Less e1 e2 -> let (t1, supply1) = newTemp supply
                                       (t2, supply2) = newTemp supply1
                                       (code1, supply3) = transExp e1 table t1 supply2
                                       (code2, supply4) = transExp e2 table t2 supply3
                                       code = code1 ++ code2 ++ [COND Lt t1 t2 l1 l2]
                                    in (code, supply4)
                     And e1 e2 -> let (t1, supply1) = newTemp supply
                                      (t2, supply2) = newTemp supply1
                                      (code1, supply3) = transExp e1 table t1 supply2
                                      (code2, supply4) = transExp e2 table t2 supply3
                                      code = code1 ++ code2 ++ [COND AndC t1 t2 l1 l2]
                                   in (code, supply4)
                     Or e1 e2 -> let (t1, supply1) = newTemp supply
                                     (t2, supply2) = newTemp supply1
                                     (code1, supply3) = transExp e1 table t1 supply2
                                     (code2, supply4) = transExp e2 table t2 supply3
                                     code = code1 ++ code2 ++ [COND OrC t1 t2 l1 l2]
                                  in (code, supply4)
                     -- Not e -> let (t1, supply1) = newTemp supply
                     --              (t2, supply2) = newTemp supply1
                     --              (code1, supply3) = transExp e1 table t1 supply2
                     --              (code2, supply4) = transExp e2 table t2 supply3
                     --              code = code1 ++ code2 ++ [COND  t1 t2 l1 l2]
                     --           in (code, supply4) --ISSUE: datatype COND requires 2 expressions

-- NOTE: Var Id Exp and Assign Id Exp might require a new new function, since they must return a new table (I believe)
-- TODO: The RETURN instruction, is it necessary? (since we dont have function calling)
transStm :: Stm -> Table -> Supply -> ([Instr], Supply)
transStm EmptyStm table supply = ([], supply)
transStm stm table supply = case stm of
  (If e stm) -> let (l1, supply1) = newLabel supply
                    (l2, supply2) = newLabel supply1
                    (code1, supply3) = transCond e table l1 l2 supply2
                    (code2, supply4) = transStm stm table supply3
                    code = code1 ++ [LABEL l1] ++ code2 ++ [LABEL l2]
                in (code, supply4)
  (IfElse e stm1 stm2) -> let (l1, supply1) = newLabel supply
                              (l2, supply2) = newLabel supply1
                              (l3, supply3) = newLabel supply2
                              (code1, supply4) = transCond e table l1 l2 supply3
                              (code2, supply5) = transStm stm1 table supply4
                              (code3, supply6) = transStm stm2 table supply5
                              code = code1 ++ [LABEL l1] ++ code2 ++ [JUMP l3] ++ [LABEL l2] ++ code3 ++ [LABEL l3]
                          in (code, supply4)
  (While e stm) -> let (l1, supply1) = newLabel supply
                       (l2, supply2) = newLabel supply1
                       (l3, supply3) = newLabel supply2
                       (code1, supply4) = transCond e table l2 l3 supply3
                       (code2, supply5) = transStm stm table supply4
                       code = [LABEL l1] ++ code1 ++ [LABEL l2] ++ code2 ++ [LABEL l1] ++ [LABEL l3]
                   in (code, supply5)
  (Print e) -> let (t1, supply1) = newTemp supply
                   (code1, supply2) = transExp e table t1 supply1
                   code = code1 ++ [PRINT' t1]
               in (code, supply2)
  (Return e) -> let (t1, supply1) = newTemp supply
                    (code1, supply2) = transExp e table t1 supply1
                    code = code1 ++ [RETURN' t1]
                in (code, supply2)

{-
> Generate intermediary code:
Prog [0/2] (sequence of stms, and empty)
BlkORStm [0/2] (stm or block) Unecessary, I believe (simply place the label after the statements in the generated assembly)
Statements [5/7] (var, id)
Expressions [16/19] (subexp, not, id)
Release temporary/registers (function)
Implement table for variables (plus scoping, plus relevant information) (necessary?)

> Generate assembly
-}
