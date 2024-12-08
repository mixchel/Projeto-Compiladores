{
module Parser where
import Lexer
}
%name parse Start
%name parseStms Stm
%tokentype { Token }
%error { parseError }
%nonassoc '>' '<' ">=" "<=" "==" "!="
%left '+' '-'
%left '*' '/' '%'
%left "&&" "||"
%left NEG '!'
%token

int {INT $$}
id {ID $$}
'+' {PLUS}
'-' {MINUS}
'*' {MULT}
'/' {DIV}
'%' {MOD}
'=' {ASSIGN}
">=" {GREATEREQ}
"<=" {LESSEQ}
"==" {EQUAL}
'>' {GREATER}
'<' {LESS}
"!=" {NEQUAL}
"&&" {AND}
"||" {OR}
'!' {NOT}
'(' {LPAREN}
')' {RPAREN}
'{' {LBRACE}
'}' {RBRACE}
var {VAR}
if {IF}
else {ELSE}
while {WHILE}
return {RETURN}
print {PRINT}
readln {READLN}
fun {FUN}


%%
Start : fun id '(' ')' '{' Prog '}'{Main $6}

Prog : Stm Prog {$1:$2}
     | {- empty -} {[]}

Stm : if '(' Exp ')' BlkORStm else BlkORStm {IfElse $3 $5 $7}
    | if '(' Exp ')' BlkORStm {If $3 $5}
    | while '(' Exp ')' BlkORStm {While $3 $5}
    | var id '=' Exp {Var $2 $4}
    | id '=' Exp {Assign $1 $3}
    | return Exp {Return $2}
    | print '(' Exp ')' {Print $3}

BlkORStm : Stm {$1}
         | '{' Prog '}' {Block $2}

Exp : '(' Exp ')' {SubExp $2}
    | '-' Exp %prec NEG { Negate $2 }
    | Exp '+' Exp {Plus $1 $3}
    | Exp '-' Exp {Minus $1 $3}
    | Exp '*' Exp {Times $1 $3}
    | Exp '/' Exp {Div $1 $3}
    | Exp '%' Exp {Mod $1 $3}
    | Exp '<' Exp {Less $1 $3}
    | Exp '>' Exp {Greater $1 $3}
    | Exp "<=" Exp {Lesseq $1 $3}
    | Exp ">=" Exp {Greatereq $1 $3}
    | Exp "==" Exp {Equal $1 $3}
    | Exp "!=" Exp {Nequal $1 $3}
    | Exp "&&" Exp {And $1 $3}
    | Exp "||" Exp {Or $1 $3}
    | '!' Exp {Not $2}
    | readln '(' ')'{Readln}
    | int {Int $1}
    | id {Identifier $1}

{

data AbstractSyntaxTree = Main Prog
    deriving (Show)

type Id = String
type Prog = [Stm]

data Stm = If Exp Stm
            | IfElse Exp Stm Stm
            | Var Id Exp
            | Return Exp
            | Print Exp
            | Block Prog
            | While Exp Stm
            | Assign Id Exp
            | ExpStm Exp
            | EmptyStm
            deriving (Show)

data Exp = Plus Exp Exp | Minus Exp Exp | Times Exp Exp | Div Exp Exp | Mod Exp Exp
        | Or Exp Exp | And Exp Exp | Not Exp
        | Equal Exp Exp | Nequal Exp Exp | Greatereq Exp Exp | Lesseq Exp Exp | Greater Exp Exp | Less Exp Exp
        | Int Int
        | Readln
        | SubExp Exp
        | Negate Exp
        | Identifier Id
        deriving (Show)

parseError :: [Token] -> a
parseError _ = error "Parse error"
}
