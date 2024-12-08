module AstGenerator where
import Parser (parse, Prog, AbstractSyntaxTree, parseStms, Stm)
import Lexer (alexScanTokens)
--import PrettyPrint
--import CodeGenerator
import System.Environment (getArgs)
import GHC.IO.Handle (Newline)

makeAst:: String -> AbstractSyntaxTree
makeAst = parse . alexScanTokens

--generateCode :: AbstractSyntaxTree -> []
--generateCode ast = generate ast

makeStmAst :: String -> Stm
makeStmAst xs = parseStms $ alexScanTokens xs

main:: IO()
main = do
    args <- getArgs
    let path = head args
    file <- readFile path
    let tokens = alexScanTokens file
    let ast = makeAst file
--    let code = generateCode ast
    print tokens
    putStrLn ""
    print ast
    putStrLn ""
--    print code
    --print $ prettyProg ast
