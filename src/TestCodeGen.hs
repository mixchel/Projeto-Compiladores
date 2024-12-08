import AstGenerator (makeStmAst)
import CodeGenerator 

exps = ["print(1+2)","print(1+3)", "print(1*5+4-3)"]

test :: String -> ([Instr], Supply)
test input = transStm (makeStmAst input) [] (0,0) 

testAll :: [([Instr], Supply)]
testAll = [test a |a <- exps]