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
%right ','
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
Boolean {BOOLEAN}
Int {INT}
Float {FLOAT}
String {STRING}
Char {CHAR}

%%
Prog : Stm Prog {$1:$2}
     | {- empty -} {[]}

Stm : if '(' Exp ')' Stm {If $3 $5 EmptyStm}
    | if '(' Exp ')' Stm else Stm {If $3 $5 $7}
    | while '(' Exp ')' Stm {WHILE $3 $5}
    | val id Type '=' Exp {Val $2 $3 $5}
    | var id Type '=' Exp {Var $2 $3 $5}
    | val id '=' Exp {Val $2 Undef $4}
    | var id '=' Exp {Val $2 Undef $4}
    | return Exp {Return $2}
    | '{' Prog '}' {Block $2} 

Exp : id '(' Arg ')' {FunCall $1 $2}
    | '(' Exp ')' {SubExp $2}
    | '-' Exp %prec NEG { Negate $2 }
    | Exp '+' Exp {$1 PLUS $3}
    | Exp '-' Exp {$1 MINUS $3}
    | Exp '*' Exp {$1 MULT $3}
    | Exp '/' Exp {$1 DIV $3}
    | Exp '%' Exp {$1 MOD $3}
    | Exp '<' Exp {$1 LESS $3}
    | Exp '>' Exp {$1 GREATER $3}
    | Exp "<=" Exp {$1 LESSEQ $3}
    | Exp ">=" Exp {$1 GREATEREQ $3}
    | Exp "!=" Exp {$1 NEQUAL $3}
    | Exp "&&" Exp {$1 AND $3}
    | Exp "||" Exp {$1 OR $3}
    | '!' Exp {NOT $2}
    | int {$1}
    | real {$1}
    | bool {$1}
    | str {$1}
    | char {$1}
    | true {$1}
    | false {$1}
    | id {$1}

Arg : Arg ',' Arg {$1:$2}
    | {- empty -} {[]}

Type : Boolean {$1}
     | Int {$1}
     | Float {$1}
     | String {$1}
     | Char {$1}

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
            | EmptyStm
data Exp = Plus Exp Exp | Minus Exp Exp | Times Exp Exp | Div Exp Exp | Mod Exp Exp
        | Or Exp Exp | And Exp Exp
        | Equal Exp Exp | Nequal Exp Exp | Greatereq Exp Exp | Lesseq Exp Exp | Greater Exp Exp | Less Exp Exp
        | Int |Float | Bool 
        | FunCall Id [Exp]
        | SubExp Exp
        | Negate Exp
data Type = Boolean | Int | Float | String | Char | Undef
}
