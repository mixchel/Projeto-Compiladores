module CodeGenerator where
import Data.Text
import Parser (Exp (..), Stm (..), AbstractSyntaxTree (..), Id, Prog)
import qualified Data.Map as Map

data Instr = MOVE Temp Temp
           | MOVEI Temp Int
           | OP BinOP Temp Temp Temp
           | LABEL Label
           | JUMP Label
           | COND BinOP Temp Temp Label Label
           | READLN Temp
           | PRINT' Temp
           | RETURN'
           | NEG Temp Temp
           | NOT Temp Temp
           | ARRAY
           | STORE Temp Int
           | LOAD Temp Int
    deriving Show

-- TODO find a way to change show Pos to become M[Pos]
data State = State {table:: Map.Map String Int, registerCount :: Int, labelCount :: Int, varCount :: Int}
    deriving Show

type Temp = String
type Label = String
type Table = [(String, Int)]
type Supply = (Int, Int)

initialState :: State
initialState = State {table = Map.empty , registerCount = 0, labelCount = 0, varCount = 0}

data BinOP = Sum | Sub | Mult | Divide | Modulus | Lt | Lteq | Eq | Neq | Gt | Gteq | AndC | OrC
    deriving Show

newTemp :: State -> (Temp, State)
newTemp s = ("t" ++ show temp, s {registerCount = temp +1})
    where temp = registerCount s

newLabel :: State -> (Label, State)
newLabel s = ("l" ++ show label, s {labelCount = label + 1})
    where label = labelCount s

newVariable :: Id -> State -> (Int, State)
newVariable id s = (var, s {varCount = var + 1, table = Map.insert id var map})
    where var = varCount s
          reg = "s" ++ show var
          map = table s

popTemp :: Int -> State -> State
popTemp x s = s {registerCount = currCount - x}
    where currCount = registerCount s

getPos:: Id -> State -> Int
getPos id s =
    case Map.lookup id map of
        Just pos -> pos
        Nothing -> error $ "Undefined variable " ++ id
    where map = table s

transStart :: AbstractSyntaxTree -> ([Instr], State)
transStart (Main prog) = ([LABEL "main"] ++ [ARRAY] ++ instrs, endState)
    where (instrs, endState) = transProg prog initialState

transProg:: Prog -> State -> ([Instr], State)
transProg [] s = ([], s)
transProg (x:xs) s = let (instr1, s1) = transStm x s
                         (instr2, s2) = transProg xs s1
                     in (instr1 ++ instr2, s2)

transBinExpAux :: BinOP -> Exp -> Exp -> Temp -> State -> ([Instr], State)
transBinExpAux op e1 e2 dest s
  = let (t1, s1) = newTemp s
        (t2, s2) = newTemp s1
        (code1, s3) = transExp e1 t1 s2
        (code2, s4) = transExp e2 t2 s3
        code = code1 ++ code2 ++ [OP op dest t1 t2]
    in (code, popTemp 2 s4)

transExp :: Exp -> Temp -> State -> ([Instr], State)
transExp (Negate e) dest s
  = let (t1, s1) = newTemp s
        (code1, s2) = transExp e t1 s1
        code = code1 ++ [NEG dest t1]
    in (code, popTemp 1 s2)
transExp (Not e) dest s
  = let (t1, s1) = newTemp s
        (code1, s2) = transExp e t1 s1
        code = code1 ++ [NOT dest t1]
    in (code, popTemp 1 s2)
transExp (Identifier id) dest s = let map = table s
                                      pos = getPos id s
                                  in ([LOAD dest pos], s)
transExp e dest s = case e of
  (Int n) -> ([MOVEI dest n], s)
  (Bool n) -> if show n == "True" then ([MOVEI dest 1], s) else ([MOVEI dest 0], s)
  Readln -> ([READLN dest], s)
  (Plus e1 e2) -> transBinExpAux Sum e1 e2 dest s
  (Minus e1 e2) -> transBinExpAux Sub e1 e2 dest s
  (Times e1 e2) -> transBinExpAux Mult e1 e2 dest s
  (Div e1 e2) -> transBinExpAux Divide e1 e2 dest s
  (Mod e1 e2) -> transBinExpAux Modulus e1 e2 dest s
  (SubExp e) -> transExp e dest s

transBinCondAux :: BinOP -> Exp -> Exp -> Label -> Label -> State -> ([Instr], State)
transBinCondAux op e1 e2 l1 l2 s
  = let (t1, s1) = newTemp s
        (t2, s2) = newTemp s1
        (code1, s3) = transExp e1 t1 s2
        (code2, s4) = transExp e2 t2 s3
        code = code1 ++ code2 ++ [COND op t1 t2 l1 l2]
     in (code, popTemp 2 s4)

transCondAux :: Exp -> Label -> Label -> State -> ([Instr], State)
transCondAux e l1 l2 s = let (t1, s1) = newTemp s
                             (code1, s2) = transExp e t1 s1
                             code = code1 ++ [COND AndC t1 t1 l1 l2]
                         in (code, popTemp 1 s2)

transCond:: Exp -> Label -> Label -> State -> ([Instr], State)
transCond e l1 l2 s = case e of
    Equal e1 e2 -> transBinCondAux Eq e1 e2 l1 l2 s
    Nequal e1 e2 -> transBinCondAux Neq e1 e2 l1 l2 s
    Greatereq e1 e2 -> transBinCondAux Gteq e1 e2 l1 l2 s
    Lesseq e1 e2 -> transBinCondAux Lteq e1 e2 l1 l2 s
    Greater e1 e2 -> transBinCondAux Gt e1 e2 l1 l2 s
    Less e1 e2 -> transBinCondAux Lt e1 e2 l1 l2 s
    And e1 e2 -> transBinCondAux AndC e1 e2 l1 l2 s
    Or e1 e2 -> transBinCondAux OrC e1 e2 l1 l2 s
    Bool e1 -> transCondAux e l1 l2 s
    Identifier e1 -> transCondAux e l1 l2 s
    Not e1 -> transCondAux e l2 l1 s

transStm :: Stm -> State -> ([Instr], State)
transStm EmptyStm s = ([], s)
transStm stm s = case stm of
  (If e stm) -> let (l1, s1) = newLabel s
                    (l2, s2) = newLabel s1
                    (code1, s3) = transCond e l1 l2 s2
                    (code2 , s4) = transStm stm s3
                    code = code1 ++ [LABEL l1] ++ code2 ++ [LABEL l2]
                in (code, s4)
  (IfElse e stm1 stm2) -> let (l1, s1) = newLabel s
                              (l2, s2) = newLabel s1
                              (l3, s3) = newLabel s2
                              (code1, s4) = transCond e l1 l2 s3
                              (code2, s5) = transStm stm1 s4
                              (code3, s6) = transStm stm2 s5
                              code = code1 ++ [LABEL l1] ++ code2 ++ [JUMP l3] ++ [LABEL l2] ++ code3 ++ [LABEL l3]
                          in (code, s6)
  (While e stm) -> let (l1, s1) = newLabel s
                       (l2, s2) = newLabel s1
                       (l3, s3) = newLabel s2
                       (code1, s4) = transCond e l2 l3 s3
                       (code2, s5) = transStm stm s4
                       code = [LABEL l1] ++ code1 ++ [LABEL l2] ++ code2 ++ [JUMP l1] ++ [LABEL l3]
                   in (code, s5)
  (Print e) -> let (t1, s1) = newTemp s
                   (code1, s2) = transExp e t1 s1
                   code = code1 ++ [PRINT' t1]
               in (code, popTemp 1 s2)
  Return -> ([RETURN'], s)
  (Block prog) -> let (code1, scopeS) = transProg prog s
                  in (code1, s{labelCount = labelCount scopeS}) -- Discard everything but the label count
  (Var id t e) -> let (t1, s1) = newTemp s
                      (code1, s2) = transExp e t1 s1
                      (pos, s3) = newVariable id s2
                      code = code1 ++ [STORE t1 pos]
                  in (code, popTemp 1 s3)
  (Assign id e) -> let (t1, s1) = newTemp s
                       (code1, s2) = transExp e t1 s1
                       pos = getPos id s2
                   in (code1 ++ [STORE t1 pos], popTemp 1 s2)
