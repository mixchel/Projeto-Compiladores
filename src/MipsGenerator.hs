module MipsGenerator where
import Data.Text
import CodeGenerator (Instr (..), BinOP (..))

type AssemblyInstr = [String]

-- TODO: do ANDC and ORC work right? (due to their bitwise nature) (perhaps use chaining)
transInstr :: Instr -> AssemblyInstr
transInstr (MOVE t1 t2) = ["addi $" ++ t1 ++ ", " ++ t2 ++", $zero"]
transInstr (MOVEI t i) = ["addi $" ++ t ++ ", " ++ show i ++ ", $zero"]
transInstr (OP op t1 t2 t3) = case op of
  Sum -> ["add $" ++ t1 ++ ", $" ++ t2 ++ ", $" ++ t3]
  Sub -> ["sub $" ++ t1 ++ ", $" ++ t2 ++ ", $" ++ t3]
  Mult -> ["mul $" ++ t1 ++ ", $" ++ t2 ++ ", $" ++ t3]
  Divide -> ["div $" ++ t1 ++ ", $" ++ t2 ++ ", $" ++ t3]
transInstr (LABEL l) = [l ++ ": "]
transInstr (JUMP l) = ["j " ++ l]
transInstr (COND op t1 t2 l1 l2) = case op of
  Eq -> ["beq $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l1,
         "j " ++ l2]
  Neq -> ["bne $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l1,
          "j " ++ l2]
  Gteq -> ["bge $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l1,
           "j " ++ l2]
  Lteq -> ["ble $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l1,
           "j " ++ l2]
  Gt -> ["bgt $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l1,
         "j " ++ l2]
  Lt -> ["blt $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l1,
         "j " ++ l2]
  -- ISSUE: not working, somehow
  -- AndC -> ["beq $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l1, --TODO
  --          "j " ++ l2]
  -- OrC -> ["beq $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l1, --TODO
  --         "j " ++ l2]
transInstr READLN = ["li $v0, 5",
                     "syscall"] --TODO: returnar valor
transInstr (PRINT' t) = ["la $a0, $" ++ t,
                         "li $v0, 1",
                         "syscall"]
transInstr (RETURN' t) = ["add $v0, $" ++ t ++ ", $zero",
                          "jr $ra"]
transInstr (NEG t) = ["subu $" ++ t ++ ", $zero, $" ++ t]
--transInstr (NOT t) = ["sltu $" ++ t ++ ", $zero, $" ++ t,
--                      "xori $" ++ t ++ ", $" ++ t ++ ", 1"]
