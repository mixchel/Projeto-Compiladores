module CodeGenerator where
import Data.Text
-- ISSUE: importing stuff from the parser generaters a parser.hs error

data Instr = MOVE Temp Temp
           | MOVEI Temp Int
           | OP BinOP Temp Temp Temp
           | OPI BinOP Temp Temp Int
           | LABEL Label
           | JUMP Label
           | COND Temp BinOP Temp Label Label
           | READLN
           | PRINT' Temp
           | RETURN' Temp
           | NEG Temp

data Stm = If Exp Stm
            | IfElse Exp Stm Stm -- TODO: maybe merge with If Exp Stm, using empty Statements, but I'm unsure how to do that
            | Var Id Exp
            | Return Exp
            | Print Exp
            | Block Prog
            | While Exp Stm
            | Assign Id Exp
            | ExpStm Exp
            | EmptyStm

-- data Exp = Plus Exp Exp | Minus Exp Exp | Times Exp Exp | Div Exp Exp | Mod Exp Exp
--         | Or Exp Exp | And Exp Exp | Not Exp
--         | Equal Exp Exp | Nequal Exp Exp | Greatereq Exp Exp | Lesseq Exp Exp | Greater Exp Exp | Less Exp Exp
--         | Int Integer | Bool Bool | Str String
--         | Readln
--         | SubExp Exp
--         | Negate Exp
--         | Identifier Id

-- ISSUE: Had to change Int Integer to Int Int so transExp (Int n) didn't raise a type error due to expecting an Int but getting an Integer
-- ISSUE: Had to transform the table this way to remove duplication
data Exp = Op BinOP Exp Exp
        | Int Int | Bool Bool | Str String
        | Readln
        | SubExp Exp
        | Negate Exp
        | Identifier Id

-- ISSUE: BinOP names conflict with data Exp operators
data BinOP = Sum | Sub | Mult | Divide | Modulus | Lt | Lteq | Eq | Neq | Gt | Gteq

type Prog = [Stm]

type Temp = String
type Id = String
type Label = String
type Table = [(String, Int)]

type Supply = (Int, Int)

newTemp :: Supply -> (Temp, Supply)
newTemp (temps, labels) = ("t"++show temps, (temps +1, labels))

newLabel :: Supply -> (Label, Supply)
newLabel (temps, labels) = ("L"++show labels, (temps, labels+1))

-- ISSUE: n is identified as an integer, for some reason??
-- TODO: bool, string, subexp, identifier
transExp :: Exp -> Table -> Temp -> Supply -> ([Instr], Supply)
transExp (Int n) table dest supply = ([MOVEI dest n], supply)
transExp Readln table dest supply = ([READLN], supply)
transExp (Op op e1 e2) table dest supply
  = let (t1, supply1) = newTemp supply
        (t2, supply2) = newTemp supply1
        (code1, supply3) = transExp e1 table t1 supply2
        (code2, supply4) = transExp e2 table t2 supply3
        code = code1 ++ code2 ++ [OP op dest t1 t2]
    in (code, supply4)
transExp (Negate e) table dest supply
  = let (t1, supply1) = newTemp supply
        (code1, supply2) = transExp e table t1 supply1
        code = code1 ++ [NEG t1]
    in (code, supply2)

-- ISSUE: transCond needs to know the Relational Operator to supply it to the generated intermediary code (relop)
-- ISSUE: transCond will accept any operator (relational or arithmetic)
transCond :: Exp -> Table -> Label -> Label -> Supply -> ([Instr], Supply)
transCond e table l1 l2 supply = case e of
                     Op op e1 e2 -> let (t1, supply1) = newTemp supply
                                        (t2, supply2) = newTemp supply1
                                        (code1, supply3) = transExp e1 table t1 supply2
                                        (code2, supply4) = transExp e2 table t2 supply3
                                        code = code1 ++ code2 ++ [COND t1 op t2 l1 l2]
                                    in (code, supply4)

-- TODO:  Var Id Exp, Block Prog, Assign Id Exp, ExpStm Exp
-- NOTE: Var Id Exp and Assign Id Exp might require a new new function, since they must return a new table (I believe)
-- TODO: The RETURN instruction, is it necessary? (since we dont have function calling)
transStm :: Stm -> Table -> Supply -> ([Instr], Supply)
transStm EmptyStm table supply = ([], supply)
transStm stm table supply = case stm of
  (If e stm1) -> let (l1, supply1) = newLabel supply
                     (l2, supply2) = newLabel supply1
                     (code1, supply3) = transCond e table l1 l2 supply2
                     (code2, supply4) = transStm stm1 table supply3
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
Generate intermediary code
Statements [0/9 rules]
Expressions [0/21]
Prog
Release temporary/registers (function)
Implement table for variables (plus scoping, plus relevant information)

Generate assembly
-}
