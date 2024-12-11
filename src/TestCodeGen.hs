module TestCodeGen where
import CodeGenerator ( Supply, Instr, transStm', initialState, State, transStart, transProg, transCond')
import Lexer (alexScanTokens,Token)
import Parser (AbstractSyntaxTree, Prog, parseStms, parse)
import MipsGenerator (transInstr)
import SemanticAnalysis (typeCheckCode, getVarOpsCode, VarOp)
import System.Environment (getArgs)


data Result = Result {tokens :: [Token], kotlinCode :: String, ast :: AbstractSyntaxTree, code :: [Instr], state :: State, assembly :: [String], types :: [VarOp], correct ::Bool}
    deriving Show

makeAst:: String -> AbstractSyntaxTree
makeAst = parse . alexScanTokens

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



