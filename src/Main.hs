module Main where
import TestCodeGen (printResult, testProg, Result (assembly))
import System.Environment (getArgs)
import System.FilePath.Posix (replaceExtension, combine)
import System.Directory (getCurrentDirectory)

main :: IO()
main = 
    do 
        args <- getArgs 
        if not (null args) then do
            let path = head args
            let outputPath = replaceExtension path ".asm"
            file <- readFile path
            let result = testProg file
            sequence_ (printResult result)
            let asm = unlines (assembly result)
            writeFile outputPath asm
        else do
            file <- getContents
            cwd <- getCurrentDirectory
            let outputPath = combine cwd "out.asm"
            let result = testProg file
            sequence_ (printResult result)
            let asm = unlines (assembly result)
            writeFile outputPath asm
