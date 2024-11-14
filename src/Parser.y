{
module Parser where
import Lexer
}
%name parse Start
%name parseStms Prog
%tokentype { Token }
%error { parseError }
%nonassoc '>' '<' ">=" "<=" "==" "!="
%left '+' '-'
%left '*' '/' '%'
%left "&&" "||"
%left NEG '!'
%token

int {INT $$}
real {REAL $$}
bool {BOOL $$}
str {STRING $$}
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
',' {COMMA}
':' {COLON}
var {VAR}
val {VAL}
if {IF}
else {ELSE}
while {WHILE}
return {RETURN}
Boolean {TBOOL}
Int {TINT}
String {TSTRING}
fun {FUN}
endStm {ENDOFSTATEMENT}


%%
Start : fun id '(' ')' '{' Prog '}'{Main $6}

Prog : Stm endStm Prog {$1:$3}
     | endStm Prog {$2}
     | Stm '{' Prog '}' Prog {$1:(Block $3):$5}
     | '{' Prog '}' Prog {Block $2:$4}
     | {- empty -} {[]}

Stm : if '(' Exp ')' BlkORStm else BlkORStm {If $3 $5 $7}
    | if '(' Exp ')' BlkORStm {If $3 $5 EmptyStm}
    | if '(' Exp ')' endStm BlkORStm else BlkORStm {If $3 $6 $8}
    | if '(' Exp ')' endStm BlkORStm {If $3 $6 EmptyStm}
    | while '(' Exp ')' BlkORStm {While $3 $5}
    | while '(' Exp ')' endStm BlkORStm {While $3 $6}
    | val id '=' Exp {Val $2 Undef $4}
    | var id '=' Exp {Val $2 Undef $4}
    | id '=' Exp {Assign $1 $3}
    | return Exp {Return $2}
    | Exp {ExpStm $1}

BlkORStm : Stm {$1}
         | '{' Prog '}' {Block $2}

Exp : id '(' Arg ')' {FunCall $1 $3}
    | '(' Exp ')' {SubExp $2}
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
    | int {Int $1}
    | bool {Bool $1}
    | str {Str $1}
    | id {Identifier $1}

Arg : Exp Arg1 {$1:$2}
    | {- empty -} {[]}

Arg1 : ',' Arg {$2}
     | {- empty -} {[]}

Type : Boolean {TBoolean}
     | Int {TInt}
     | String {TString}

{

data AbstractSyntaxTree = Main Prog
    deriving (Show)

type Id = String
type Prog = [Stm]

data Stm = If Exp Stm Stm
            | Var Id Type Exp
            | Val Id Type Exp
            | Return Exp
            | Block Prog
            | While Exp Stm
            | Assign Id Exp
            | ExpStm Exp
            | EmptyStm
            deriving (Show)

data Exp = Plus Exp Exp | Minus Exp Exp | Times Exp Exp | Div Exp Exp | Mod Exp Exp
        | Or Exp Exp | And Exp Exp | Not Exp
        | Equal Exp Exp | Nequal Exp Exp | Greatereq Exp Exp | Lesseq Exp Exp | Greater Exp Exp | Less Exp Exp
        | Int Integer | Bool Bool | Str String
        | FunCall Id [Exp]
        | SubExp Exp
        | Negate Exp
        | Identifier Id
        deriving (Show)

data Type = TBoolean | TInt | TString | Undef
            deriving (Show)

parseError :: [Token] -> a
parseError _ = error "Parse error"
}
