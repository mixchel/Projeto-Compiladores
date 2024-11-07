module Main where
import Parser (parse, Prog, AbstractSyntaxTree)
import Lexer (alexScanTokens)
import PrettyPrint
import System.Environment (getArgs)


makeAst:: String -> AbstractSyntaxTree
makeAst = parse . alexScanTokens

main:: IO()
main = do
    args <- getArgs
    let path = head args
    file <- readFile path
    let tokens = alexScanTokens file
    let ast = makeAst file
    print tokens
    putStrLn ""
    print ast
    putStrLn ""
    --print $ prettyProg ast
