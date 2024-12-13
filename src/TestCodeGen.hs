module TestCodeGen where
import AstGenerator (makeStmAst, makeAst)
import CodeGenerator ( Supply, Instr, transStm, initialState, State, transStart, transProg, transCond)
import Lexer (Token, alexScanTokens)
import Parser (AbstractSyntaxTree, Prog, parseStms, parse)
import MipsGenerator (transInstr)
import SemanticAnalysis (typeCheckCode, getVarOpsCode, VarOp)
import System.Environment (getArgs)


-- TODO: add tests for while, CONDs with arithmetic, READLN, var assignment, scopes, types, arithmetic with immediate values and w/vars
stms :: [String]
stms = ["print(1+2)",
        "print(1*5+4-3)",
        "print(1+5*8)",
        "print(1*(5-1))",
        "print((5-1)*4)",
        "if (5<6) return 5 else return 2",
        "if (5<2) return 5 else {print(1) print(2)}",
        "if (5<2) {print(4) return 5} else return 3",
        "if (5<2) {if (5>2) return 4 if (5<3) return 5} else return 3",
        "return 100"]


data Result = Result {tokens :: [Token], kotlinCode :: String, ast :: AbstractSyntaxTree, code :: [Instr], state :: State, assembly :: [String], types :: [VarOp], correct ::Bool}
    deriving Show

testProg :: [Char] -> Result
testProg input = Result {tokens = tokens, ast = ast, code = code, state = state, kotlinCode = input, assembly = asm, types = varOps, correct = correct'}
    where tokens = alexScanTokens input
          ast = parse tokens
          varOps = getVarOpsCode ast
          correct' = typeCheckCode varOps
          (code,state) = transStart ast
          asm = concatMap transInstr code

printCode:: [Instr] -> [IO()]
printCode = map print

newline :: IO ()
newline = putStrLn ""

printResult :: Result -> [IO ()]
printResult r = start:og:newline:to:newline:tree:newline:typeList ++ correct':newline:st:newline:c ++ newline:asm
    where og = putStrLn $ kotlinCode r
          to = print $ tokens r
          tree = print $ ast r
          typeList = map print (types r)
          correct' = print (correct r)
          st = print $ state r
          c = printCode $ code r
          start = putStrLn "\nResults:\n"
          asm = map putStrLn (assembly r)



