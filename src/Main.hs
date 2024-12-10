module Main where
import TestCodeGen (printResult, testProg, Result (assembly))
import System.Environment (getArgs)
import System.FilePath.Posix (replaceExtension)

main :: IO()
main = 
    do 
        args <- getArgs
        let path = head args
        let outputPath = replaceExtension path ".asm"
        file <- readFile path
        let result = testProg file
        sequence_ (printResult result)
        let asm = unlines (assembly result)
        writeFile outputPath asm