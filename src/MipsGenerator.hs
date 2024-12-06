module MipsGenerator where
import Data.Text
import CodeGenerator (Instr)

-- TODO: define asembly datatype
--data Assembly = 

transInstr :: Instr -> Assembly
transInstr MOVE t1 t2 = addi t1, t2, $zero
transInstr MOVEI t i = addi t, i, $zero
--transInstr OP op t1 t2 t3
--transInstr OPI op t1 t2 i
transInstr LABEL l = l ++ ": "
transInstr JUMP l = j l
transInstr COND Temp BinOP Temp Label Label
-- TODO: this implementation of READLN only works with strings
transInstr READLN = la $a0, target
                    li $v0, 8
                    syscall
-- TODO: this implementation of READLN only works with strings
transInstr PRINT' t = la $a0, target
                      li $v0, 4
                      syscall
transInstr RETURN' t = add $v0, t, $zero
                       jr $ra
transInstr NEG t = subu t, $zero, t
