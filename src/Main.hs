import Parser (parse, Prog)
import Lexer (alexScanTokens)
import System.Environment (getArgs)

ast:: String -> Prog
ast = parse . alexScanTokens

main:: IO()
main = do
    args <- getArgs
    let path = head args
    file <- readFile path
    print $ alexScanTokens file
    print $ ast file