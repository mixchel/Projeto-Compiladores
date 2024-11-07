module PrettyPrint where
import Data.Text (Text, pack)
import Parser 
import Data.Sequence (Seq(Empty))

unquote:: String -> String
unquote = filter (/= '\"')

prettyProg :: [Parser.Stm] -> Text
prettyProg = foldr ((<>) . prettyStm) (pack "")

prettyType :: Type -> Text
prettyType x = pack (show x)

prettyArg :: [Exp] -> Text
prettyArg = foldr ((<>) . prettyExp) (pack "")

prettyStm :: Stm -> Text
prettyStm (If e1 s1 EmptyStm) = pack "If"  <> pack "(" <> prettyExp e1 <> pack ")" <> prettyStm s1
prettyStm (If e1 s1 s2) = pack "If" <> pack "(" <> prettyExp e1 <> pack ")" <> prettyStm s1 <>pack "else" <> prettyStm s2
prettyStm (While e1 s1) = pack "While" <> pack "(" <> prettyExp e1 <> pack ")" <> prettyStm s1
prettyStm (Val id Undef e1) = pack "val" <> pack id <> pack "=" <> prettyExp e1
prettyStm (Var id Undef e1) = pack "var" <> pack id <> pack "=" <> prettyExp e1
prettyStm (Val id t e1) = pack "val" <> pack id <> pack ":" <> prettyType t <> pack "=" <> prettyExp e1
prettyStm (Var id t e1) = pack "var" <> pack id <> pack ":" <> prettyType t <> pack "=" <> prettyExp e1
prettyStm (Assign id e1) = pack id <> pack "=" <> prettyExp e1
prettyStm (Return e1) = pack "Return" <> prettyExp e1
prettyStm (Block prog) = pack "{" <> prettyProg prog <> pack "}"
prettyStm (ExpStm (FunCall id arg)) = pack id <> pack "(" <> prettyArg arg <> pack ")"

prettyExp :: Exp -> Text
prettyExp (FunCall id arg) = pack id <> pack "(" <> prettyArg arg <> pack ")"
prettyExp (SubExp e1) = pack "(" <> prettyExp e1 <> pack ")"
prettyExp (Negate e1) = pack "-" <> prettyExp e1
prettyExp (Plus e1 e2) = pack "+" <> prettyExp e1 <> prettyExp e2
prettyExp (Minus e1 e2) = pack "-" <> prettyExp e1 <> prettyExp e2
prettyExp (Times e1 e2) = pack "*" <> prettyExp e1 <> prettyExp e2
prettyExp (Div e1 e2) = pack "/" <> prettyExp e1 <> prettyExp e2
prettyExp (Mod e1 e2) = pack "%" <> prettyExp e1 <> prettyExp e2
prettyExp (Less e1 e2) = pack "<" <> prettyExp e1 <> prettyExp e2
prettyExp (Greater e1 e2) = pack ">" <> prettyExp e1 <> prettyExp e2
prettyExp (Lesseq e1 e2) = pack "=<" <> prettyExp e1 <> prettyExp e2
prettyExp (Greatereq e1 e2) = pack ">=" <> prettyExp e1 <> prettyExp e2
prettyExp (Nequal e1 e2) = pack "!=" <> prettyExp e1 <> prettyExp e2
prettyExp (And e1 e2) = pack "&&" <> prettyExp e1 <> prettyExp e2
prettyExp (Or e1 e2) = pack "||" <> prettyExp e1 <> prettyExp e2
prettyExp (Not e1) = pack "!" <> prettyExp e1

-- ??? Terminais, simplesmente fazer print dos seus valores ???
prettyExp (Int e) = pack $ unquote $ show e
prettyExp (Float e) = pack $ unquote $ show e
prettyExp (Bool e) = pack $ unquote $ show e
prettyExp (Str e) = pack $ unquote $ show e
prettyExp (Char e) = pack $ unquote $ show e
prettyExp (Identifier e) = pack $ unquote $ show e