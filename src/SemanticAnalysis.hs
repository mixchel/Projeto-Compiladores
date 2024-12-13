module SemanticAnalysis where
import Parser
    ( Exp(..),
      Id,
      Prog,
      Stm(..))
import qualified Parser
import qualified Data.Set as Set
import qualified Data.Map as Map


type ExpType = (Type, Set.Set Id)
data VarOp = Declaration Id Type ExpType | Assignment Id ExpType | Scope [VarOp] | Condition ExpType
    deriving Show
data Type = Undef | Invalid | TInt | TBool
    deriving (Show, Eq)
getVarOpsCode:: Parser.AbstractSyntaxTree -> [VarOp]
getVarOpsCode (Parser.Main prog) = getVarOps prog

getVarOps :: Prog -> [VarOp]
getVarOps = concatMap getVarOpsStm
type TypeTable = Map.Map Id Type

typeCheckCode:: [VarOp] -> Bool
typeCheckCode = fst . typeCheck Map.empty 

typeCheck  :: TypeTable -> [VarOp] -> (Bool, TypeTable)
typeCheck table [] = (True, table)
typeCheck table (x:xs) = (correct1 && correct2, table2)
    where (correct1,table1) = typeCheckStm x table
          (correct2, table2) = typeCheck table1 xs

typeCheckStm :: VarOp -> TypeTable -> (Bool, TypeTable)
typeCheckStm (Declaration id type' expType) table = (typeCheckExp table type' expType, table1)
    where table1 = Map.insert id type' table
typeCheckStm (Assignment id expType) table = (typeCheckVar type' table id && typeCheckExp table type'  expType, table)
    where type' = getType table id     
typeCheckStm (Condition (expType, vars)) table = (typeCheckVars TInt table vars,table)
typeCheckStm (Scope xs) table = (correct, table) 
    where (correct, _) = typeCheck table xs 

getType:: TypeTable -> Id -> Type
getType table id = case Map.lookup id table of
    Just t -> t
    Nothing -> error "Variable not declared" 

typeCheckExp :: TypeTable -> Type -> (Type, Set.Set Id) -> Bool
typeCheckExp table type' (exptype, vars) = type' == exptype && typeCheckVars exptype table vars


typeCheckVars:: Type -> TypeTable -> Set.Set Id -> Bool
typeCheckVars type' table vars = all (typeCheckVar type' table) (Set.toList vars)

typeCheckVar :: Type ->  TypeTable -> String -> Bool
typeCheckVar type' table var =
    case Map.lookup var table of
    Just vartype -> type' == vartype
    Nothing -> False

getVarOpsStm :: Stm -> [VarOp]
getVarOpsStm (If exp stm) = Condition (expTypes exp) : getVarOpsStm stm
getVarOpsStm (IfElse exp stm1 stm2) = Condition (expTypes exp) : getVarOpsStm stm1 ++ getVarOpsStm stm2
getVarOpsStm (Var id Parser.TInt e) = [Declaration id TInt (expTypes e)]
getVarOpsStm (Print e) = [Condition (expTypes e)]
getVarOpsStm (Assign id e) = [Assignment id (expTypes e)]
getVarOpsStm (Block p) = [Scope (getVarOps p)]
getVarOpsStm (While exp stm) = Condition (expTypes exp) : getVarOpsStm stm
getVarOpsStm x = []

expTypes2 :: Exp -> Exp -> (Type, Set.Set Id)
expTypes2 e1 e2
    | t1 == Undef = (t2, s)
    | t2 == Undef = (t1, s)
    | t1 == Invalid || t2 == Invalid || t1 /= t2 = (Invalid, Set.empty)
    | otherwise = (t1, s)
    where (t1,s1) = expTypes e1
          (t2,s2) = expTypes e2
          s = s1 `Set.union` s2

expTypes :: Exp -> (Type, Set.Set Id)
expTypes (Plus e1 e2)
        | t == TInt || t == Undef = (TInt,s)
        | otherwise = (Invalid, Set.empty)
    where (t,s) = expTypes2 e1 e2
expTypes (Minus e1 e2)
        | t == TInt || t == Undef = (TInt,s)
        | otherwise = (Invalid, Set.empty)
    where (t,s) = expTypes2 e1 e2
expTypes (Times e1 e2)
        | t == TInt || t == Undef = (TInt,s)
        | otherwise = (Invalid, Set.empty)
    where (t,s) = expTypes2 e1 e2
expTypes (Div e1 e2)
        | t == TInt || t == Undef = (TInt,s)
        | otherwise = (Invalid, Set.empty)
    where (t,s) = expTypes2 e1 e2
expTypes (Mod e1 e2)
        | t == TInt || t == Undef = (TInt,s)
        | otherwise = (Invalid, Set.empty)
    where (t,s) = expTypes2 e1 e2
expTypes (Equal e1 e2)
        | t == TInt || t == Undef = (TBool,s)
        | otherwise = (Invalid, Set.empty)
    where (t,s) = expTypes2 e1 e2
expTypes (Nequal e1 e2)
        | t == TInt || t == Undef = (TBool,s)
        | otherwise = (Invalid, Set.empty)
    where (t,s) = expTypes2 e1 e2
expTypes (Greatereq e1 e2)
        | t == TInt || t == Undef = (TBool,s)
        | otherwise = (Invalid, Set.empty)
    where (t,s) = expTypes2 e1 e2
expTypes (Lesseq e1 e2)
        | t == TInt || t == Undef = (TBool,s)
        | otherwise = (Invalid, Set.empty)
    where (t,s) = expTypes2 e1 e2
expTypes (Greater e1 e2)
        | t == TInt || t == Undef = (TBool,s)
        | otherwise = (Invalid, Set.empty)
    where (t,s) = expTypes2 e1 e2
expTypes (Less e1 e2)
        | t == TInt || t == Undef = (TBool,s)
        | otherwise = (Invalid, Set.empty)
    where (t,s) = expTypes2 e1 e2
expTypes (Or e1 e2)
        | t == TBool || t == Undef  = (TBool,s)
        | otherwise = (Invalid, Set.empty)
    where (t,s) = expTypes2 e1 e2
expTypes (And e1 e2)
        | t == TBool || t == Undef  = (TBool,s)
        | otherwise = (Invalid, Set.empty)
    where (t,s) = expTypes2 e1 e2
expTypes (Int _) = (TInt, Set.empty)
expTypes (Bool _) = (TBool, Set.empty)
expTypes (Identifier id) = (Undef, Set.singleton id)
expTypes Readln = (TInt, Set.empty)
expTypes (SubExp e) = expTypes e
expTypes (Negate e)
    | t == TInt || t == Undef = (TInt,s)
    | otherwise = (Invalid, Set.empty)
    where (t,s) = expTypes e