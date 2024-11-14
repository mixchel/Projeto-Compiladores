module CodeGenerator where
import Data.Text

data Instr = MOVE Temp Temp
           | MOVEI Temp Int
           | OP BinOP Temp Temp Temp
           | OPI BinOP Temp Temp Int
           | LABEL Label
           | JUMP Label
           | COND Temp BinOP Temp Label Label

data BinOP = Plus | Minus | Times | Div | Mod

data Exp = Num Int
         | Op BinOP Exp Exp

data Stm = Return Exp
         | Assign Id Exp
         | If (Cond) Stm
         | IfElse (Cond) Stm Else Stm
         | While (Cond) Stm
         | StmList

data Relop = Less | Greater | Lesseq | Greatereq | Equal | Nequal | And | Or

data Cond = Exp Relop Exp
data StmList = Stm StmList | Nothing

type Temp = String
type Id = String
type Label = String
type Table = [(String, Int)]

type Supply = (Int, Int)

newTemp :: Supply -> (Temp, Supply)
newTemp (temps, labels) = ("t"++show temps, (temps +1, labels))

newLabel :: Supply -> (Label, Supply)
newLabel (temps, labels) = ("L"++show labels, (temps, labels+1))

transExp :: Exp -> Table -> Temp -> Supply -> ([Instr], Supply)
transExp (Num n) table dest supply = ([MOVEI dest n], supply)
transExp (Op op e1 e2) table dest supply
  = let (t1, supply1) = newTemp supply
        (t2, supply2) = newTemp supply1
        (code1, supply3) = transExp e1 table t1 supply2
        (code2, supply4) = transExp e2 table t2 supply3
        code = code1 ++ code2 ++ [OP op dest t1 t2]
    in (code, supply4)

transCond :: Cond -> Table -> Label -> Label -> [Instr]
transCond cond table l1 l2 supply = case cond of
                     Cond e1 relop e2 -> let (t1, supply1) = newTemp supply
                                             (t2, supply2) = newTemp supply1
                                             (code1, supply3) = transExp e1 table t1 supply2
                                             (code2, supply4) = transExp e2 table t2 supply3
                                             code = code1 ++ code2 ++ [COND t1 relop t2 l1 l2]
                                             in (code, supply4)

transStm :: Stm -> Table -> Supply -> ([Instr], Supply)
transStm stm table supply = case stm of
  If (Cond) stm1 -> let (l1, supply1) = newLabel supply
                        (l2, supply2) = newLabel supply1
                        (code1, supply3) = transCond Cond table l1 l2
                        (code2, supply4) = transStm stm1 table supply3
                        code = code1 ++ [LABEL l1] ++ code2 ++ [LABEL l2]
                    in (code, supply4)

{-
TODO
Negate expression
If Else
While
Return
FuncCall (necessary?)
Table Implementation
Eval down to atoms (int, float, str, id, etc...)
-}
