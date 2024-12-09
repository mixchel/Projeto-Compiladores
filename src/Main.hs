module Main where
import TestCodeGen (printResult, testProg)
import System.Environment (getArgs)

main :: IO()
main = 
    do 
        args <- getArgs
        let path = head args
        file <- readFile path
        sequence_ (printResult (testProg file))