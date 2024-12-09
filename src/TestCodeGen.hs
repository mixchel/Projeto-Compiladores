import AstGenerator (makeStmAst)
import CodeGenerator ( Supply, Instr, transStm', initialState, State)
import Distribution.Pretty (Pretty(pretty))
import Language.Haskell.TH.Syntax (sequenceQ)

-- TODO: add tests for while, CONDs with arithmetic, READLN, var assignment, scopes, types, arithmetic with immediate values and w/vars
exps :: [String]
exps = ["print(1+2)",
        "print(1+3)",
        "print(1*5+4-3)",
        "print(1+5*8)",
        "print(1*(5-1))",
        "print((5-1)*4)",
        "if (5<6) return 5 else return 2",
        --"if (5+6) return 5 else return 2",
        --"if (5<2) return 5 else {print(1) print(2)}",
        "print(2)"]

test :: String -> ([Instr], State)
test input = transStm' (makeStmAst input) initialState

testAll :: [([Instr], State)]
testAll = map test exps

prettyTest :: [Char] -> [IO ()]
prettyTest input = print input:newline:print ast:newline:code
    where ast = makeStmAst input
          (instr, _) = transStm' ast initialState
          code = map print instr
newline = putStrLn ""  

prettyTestAll :: [[IO ()]]
prettyTestAll = map prettyTest exps

seqNl :: [IO ()] -> IO ()
seqNl a = sequence_ (a ++ [putStrLn ""]) 


main :: IO()
main =
    mapM_ seqNl prettyTestAll
