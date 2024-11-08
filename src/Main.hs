module Main where
import Parser (parse, Prog, AbstractSyntaxTree, parseStms)
import Lexer (alexScanTokens, Token (RBRACE, NEWLINE, ENDOFSTATEMENT, SEMICOLON, ELSE))
import PrettyPrint
import System.Environment (getArgs)
import GHC.IO.Handle (Newline)


wrapRbrace :: [Token] -> [Token]
wrapRbrace (RBRACE:xs) = NEWLINE:RBRACE:wrapRbrace xs
wrapRbrace (x:xs) = x : wrapRbrace xs
wrapRbrace [] = []

mergeEndofStm :: [Token] -> [Token]
mergeEndofStm (NEWLINE:xs) = ENDOFSTATEMENT: mergeEndofStm xs
mergeEndofStm (SEMICOLON:xs) = ENDOFSTATEMENT: mergeEndofStm xs
mergeEndofStm (x:xs) = x : mergeEndofStm xs
mergeEndofStm [] = []

removeIrrelevantNewlines :: [Token] -> [Token]
removeIrrelevantNewlines (NEWLINE:ELSE:NEWLINE:xs) = ELSE: removeIrrelevantNewlines xs
removeIrrelevantNewlines (NEWLINE:ELSE:xs) = ELSE: removeIrrelevantNewlines xs
removeIrrelevantNewlines (ELSE:NEWLINE:xs) = ELSE: removeIrrelevantNewlines xs
removeIrrelevantNewlines (NEWLINE:NEWLINE:xs) = NEWLINE: removeIrrelevantNewlines xs
removeIrrelevantNewlines (x:xs) = x :removeIrrelevantNewlines xs
removeIrrelevantNewlines [] = []

prepareTokens :: [Token] -> [Token]
prepareTokens = mergeEndofStm . removeIrrelevantNewlines . wrapRbrace

makeAst:: String -> AbstractSyntaxTree
makeAst = parse . prepareTokens . alexScanTokens

makeStmAst :: String -> Prog
makeStmAst xs = parseStms $ prepareTokens (alexScanTokens xs ++ [ENDOFSTATEMENT])

main:: IO()
main = do
    args <- getArgs
    let path = head args
    file <- readFile path
    let tokens = prepareTokens $ alexScanTokens file
    let ast = makeAst file
    print tokens
    putStrLn ""
    print ast
    putStrLn ""
    --print $ prettyProg ast
