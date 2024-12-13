module TestCodeGen where
import CodeGenerator ( Supply, Instr, transStm, initialState, State, transStart, transProg, transCond)
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
printResult r = let
    og = [putStrLn $ kotlinCode r]
    to = putStrLn "\n------Tokens: ":[print $ tokens r]
    tree = putStrLn "\n------Ast:": [print $ ast r]
    typeList = putStrLn "\n------Type Checking:" : map print (types r)
    correct' = putStr "Are Types Correct: ": [print $ correct r]
    st = putStrLn "\n------Final State: " : [print $ state r]
    c = putStrLn "\n------intermediary Code:":printCode (code r)
    asm = putStrLn "Assembly code:": map putStrLn (assembly r)
    in og ++ to ++ tree ++ typeList ++ correct' ++ st ++ c ++ asm
