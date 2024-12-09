module TestCodeGen where
import AstGenerator (makeStmAst, makeAst)
import CodeGenerator ( Supply, Instr, transStm', initialState, State, transStart, transProg)
import Lexer (Token, alexScanTokens)
import Parser (AbstractSyntaxTree, Prog, parseStms, parse)
import MipsGenerator (transInstr)
import System.Environment (getArgs)

-- TODO: add tests for while, CONDs with arithmetic, READLN, var assignment, scopes, types, arithmetic with immediate values and w/vars
stms :: [String]
stms = ["print(1+2)",
        "print(1+3)",
        "print(1*5+4-3)",
        "print(1+5*8)",
        "print(1*(5-1))",
        "print((5-1)*4)",
        "if (5<6) return 5 else return 2",
        --"if (5+6) return 5 else return 2",
        "if (5<2) return 5 else {print(1) print(2)}",
        "if (5<2) {print(4) return 5} else return 3",
        "if (5<2) {if (5>2) return 4 if (5<3) return 5} else return 3",
        "return 100"]
      

data Result a = Result {tokens :: [Token], kotlinCode :: String, ast :: a, code :: [Instr], state :: State, assembly :: [String]}
    deriving Show

test :: ([Token] -> a) -> (a -> ([Instr], State)) -> [Char] -> Result a
test parse trans input = Result {tokens = tokens, ast = ast, code = code, state = state, kotlinCode = input, assembly = asm}
    where tokens = alexScanTokens input
          ast = parse tokens
          (code,state) = trans ast
          asm = transInstr code 

testStms :: [Char] -> Result Prog
testStms = test parseStms (transProg initialState)

testProg :: [Char] -> Result AbstractSyntaxTree
testProg = test parse transStart             

printCode:: [Instr] -> [IO()]
printCode = map print

newline :: IO ()
newline = putStrLn ""

printResult :: Show a => Result a -> [IO ()]
printResult r = start:og:newline:to:newline:tree:newline:st:newline:c ++ newline:asm
    where og = putStrLn $ kotlinCode r
          to = print $ tokens r
          tree = print $ ast r
          st = print $ state r
          c = printCode $ code r
          start = putStrLn "\nResults:\n"
          asm = map print (assembly r)
          

getResult :: [Char] -> [IO ()]
getResult = printResult . testStms 

testAll :: IO ()
testAll = sequence_ $ concatMap getResult stms
