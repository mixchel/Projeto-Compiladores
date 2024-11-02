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
%right COMP ','
%token

int {INT $$}
real {REAL $$}
bool {BOOL $$}
str {STRING $$}
char {CHAR $$}
true {TRUE}
false {FALSE}
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
'{' {LBRACKET}
'}' {RBRACKET}
',' {COMMA}
var {VAR}
val {VAL}
if {IF}
else {ELSE}
while {WHILE}
return {RETURN}
"Boolean" {BOOLEAN}
"Int" {INT}
"Float" {FLOAT}
"String" {STRING}
"Char" {CHAR}
"Undef" {UNDEF}

%%
Prog : Prog Prog %prec COMP {$1:$2}
    | {- empty -} {[]}
    | Com {$1}

Com : if '(' Exp ')' Com {If $3 $5}
    | while '(' Exp ')' Com {WHILE $3 $5}
    | val id Type '=' Exp {Val $2 $3 $5}
    | var id Type '=' Exp {Var $2 $3 $5}
    | val id '=' Exp {Val $2 Undef $4}
    | var id '=' Exp {Val $2 Undef $4}
    | return Exp {Return $2}
    | '{' Prog '}' {Block $2} 

Exp : id '(' Arg ')' {FunCall $1 $2}
    | '(' Exp ')' {SubExp $2}
    | '-' Exp %prec NEG { Negate $2 }

Arg : Arg ',' Arg {$1:$2}
    | {- empty -} {[]}

Type : "Boolean" {$1}
     | "Int" {$1}
     | "Float" {$1}
     | "String" {$1}
     | "Char" {$1}
     | "Undef" {$1}

{
type Id = String
type Prog = [Com]
data Com = IF Exp Com Com 
            | Var Id Type Exp 
            | Val Id Type Exp 
            | Return Exp 
            | Block Prog
            | While Exp Com
            | Assign Id Exp
data Exp = Plus Exp Exp | Minus Exp Exp | Times Exp Exp | Div Exp Exp | Mod Exp Exp
        | Or Exp Exp | And Exp Exp
        | Equal Exp Exp | Nequal Exp Exp | Greatereq Exp Exp | Lesseq Exp Exp | Greater Exp Exp | Less Exp Exp
        | Int |Float | Bool 
        | FunCall Id [Exp]
        | SubExp Exp
        | Negate Exp
data Type = Boolean | Int | Float | String | Char | Undef
}
