module MipsGenerator where
import Data.Text
import CodeGenerator (Instr (..), BinOP (..))

type AssemblyInstr = [String]

transInstr :: Instr -> AssemblyInstr
transInstr (MOVE t1 t2) = ["add $" ++ t1 ++ ", " ++ t2 ++", $zero"]
transInstr (MOVEI t i) = ["li $" ++ t ++ ", " ++ show i]
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
  AndC -> ["bne $" ++ t1 ++ ", 1, " ++ l2,
           "bne $" ++ t2 ++ ", 1, " ++ l2,
           "j " ++ l1]
  OrC -> ["beq $" ++ t1 ++ ", 1, " ++ l1,
          "beq $" ++ t2 ++ ", 1, " ++ l1,
          "j " ++ l2]
transInstr (READLN t) = ["li $v0, 5",
                         "syscall",
                         "move $" ++ t ++ ", $v0"]
transInstr (PRINT' t) = ["move $a0, $" ++ t,
                         "li $v0, 1",
                         "syscall"]
transInstr RETURN' = ["li $v0, 10",
                      "syscall"]
transInstr (NEG t) = ["subu $" ++ t ++ ", $zero, $" ++ t]
transInstr (NOT t) = ["sltu $" ++ t ++ ", $zero, $" ++ t,
                      "xori $" ++ t ++ ", $" ++ t ++ ", 1"]
transInstr (STORE t i) = ["sw $" ++ t ++ ", " ++ show (i * 4) ++ "($s0)"]
transInstr (LOAD t i) = ["lw $" ++ t ++ ", " ++ show (i * 4) ++ "($s0)"]
transInstr ARRAY = ["la $s0, vars"]
