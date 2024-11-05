{
module Parser where
import Lexer
}
%name parse
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
char {CHAR $$}
id {ID $$}
'+' {PLUS}
'-' {MINUS}
'*' {MULT}
'/' {DIV}
'%' {MOD}
'=' {ASSIGN}
">=" {GREATEREQ}
"<=" {LESSEQ}
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
':' {SEMICOLON}
var {VAR}
val {VAL}
if {IF}
else {ELSE}
while {WHILE}
return {RETURN}
Boolean {TBOOL}
Int {TINT}
Float {TFLOAT}
String {TSTRING}
Char {TCHAR}

%%
Prog : Stm Prog {$1:$2}
     | {- empty -} {[]}

Stm : if '(' Exp ')' Stm else Stm {If $3 $5 $7}
    | if '(' Exp ')' Stm {If $3 $5 EmptyStm}
    | while '(' Exp ')' Stm {While $3 $5}
    | val id ':' Type '=' Exp {Val $2 $4 $6}
    | var id ':' Type '=' Exp {Var $2 $4 $6}
    | val id '=' Exp {Val $2 Undef $4}
    | var id '=' Exp {Val $2 Undef $4}
    | id '=' Exp       {Assign $1 $3}
    | return Exp {Return $2}
    | '{' Prog '}' {Block $2}
    | id '(' Arg ')' {ExpStm (FunCall $1 $3)}

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
    | Exp "!=" Exp {Nequal $1 $3}
    | Exp "&&" Exp {And $1 $3}
    | Exp "||" Exp {Or $1 $3}
    | '!' Exp {Not $2}
    | int {V (A $1)}
    | real {V (B $1)}
    | bool {V (E $1)}
    | str {V (C $1)}
    | char {V (D $1)}
    | id {Identifier $1}

Arg : Exp ',' Arg {$1:$3}
    | {- empty -} {[]}

Type : Boolean {TBoolean}
     | Int {TInt}
     | Float {TFloat}
     | String {TString}
     | Char {TChar}

{
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
data Value = A Int | B Float |C String |D Char |E Bool
            deriving (Show)
data Exp = Plus Exp Exp | Minus Exp Exp | Times Exp Exp | Div Exp Exp | Mod Exp Exp
        | Or Exp Exp | And Exp Exp | Not Exp
        | Equal Exp Exp | Nequal Exp Exp | Greatereq Exp Exp | Lesseq Exp Exp | Greater Exp Exp | Less Exp Exp
        | V Value
        | FunCall Id [Exp]
        | SubExp Exp
        | Negate Exp
        | Identifier Id
        deriving (Show)
data Type = TBoolean | TInt | TFloat | TString | TChar | Undef
            deriving (Show)

parseError :: [Token] -> a
parseError _ = error "Parse error"
}
