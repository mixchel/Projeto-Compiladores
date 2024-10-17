type Identifier = String
data AritBinOP = Plus | Minus | Mult | Div | Mod | LessEq | GreaterEq | Less | Greater | Equal
    deriving (Eq,Show)
data BoolBinOP = And | Or 
    deriving (Eq, Show)
data Exp = IdExp Identifier 
        | NumExp 
