import Parser (parse, Prog)
import Lexer (alexScanTokens)
import System.Environment (getArgs)
import Data.ByteString (readFile)

ast:: String -> Prog
ast = parse alexScanTokens

main:: IO()
main = do
    path <- head getArgs
    print ast readFile file