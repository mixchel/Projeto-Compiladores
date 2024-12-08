import AstGenerator (makeStmAst)
import CodeGenerator ( Supply, Instr, transStm )
import Distribution.Pretty (Pretty(pretty))
import Language.Haskell.TH.Syntax (sequenceQ)

exps = ["print(1+2)","print(1+3)", "print(1*5+4-3)","print(1+5*8)"]

test :: String -> ([Instr], Supply)
test input = transStm (makeStmAst input) [] (0,0)

testAll :: [([Instr], Supply)]
testAll = map test exps

prettyTest :: [Char] -> [IO ()]
prettyTest input = putStrLn input:map print instr
    where (instr,_) = test input

prettyTestAll :: [[IO ()]]
prettyTestAll = map prettyTest exps

seqNl :: [IO ()] -> IO ()
seqNl a = sequence_ (a ++ [putStrLn ""]) 


main :: IO()
main = mapM_ seqNl prettyTestAll
