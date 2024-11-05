{
module Lexer where
}

%wrapper "basic"

tokens :-

-- syntax
$white+             ;
\/\/.*$             ;
\/\*(.|\s)*\*\/     ;
\( {\s -> LPAREN}
\) {\s -> RPAREN}
\{ {\s -> LBRACE}
\} {\s -> RBRACE}
"," {\s -> COMMA}

-- types
Int {\s -> TINT}
Float {\s -> TFLOAT}
String {\s -> TSTRING}
Char {\s -> TCHAR}
Boolean {\s -> TBOOL}

-- arithmetic
\+ {\s -> PLUS}
\- {\s -> MINUS}
\* {\s -> MULT}
\/ {\s -> DIV}
\% {\s -> MOD}

-- comparison
"<=" {\s -> LESSEQ}
">=" {\s -> GREATEREQ}
"<" {\s -> LESS}
">" {\s -> GREATER}
"==" {\s -> EQUAL}
"!=" {\s -> NEQUAL}
"&&" {\s -> AND}
"||" {\s -> OR}
"!" {\s -> NOT}

-- values
[0-9]+ {\s -> INT (read s)}
[0-9]+"."[0-9]+ {\s -> REAL (read s)}
\"[a-zA-Z0-9_ ]+\" {\s -> STRING (unquote s)}
\'[a-zA-Z0-9_]\' {\s -> CHAR (s !! 1)}
true {\s -> BOOL True}
false {\s -> BOOL False}
-- flow control
if {\s -> IF}
else {\s -> ELSE}
while {\s -> WHILE}

-- symbols & keywords
var {\s -> VAR}
val {\s -> VAL}
return {\s -> RETURN}
[_a-zA-z]+[_a-zA-Z0-9]* {\s -> ID s}
":" {\s -> SEMICOLON}
"=" {\s -> ASSIGN}

{
unquote :: String -> String
unquote s = init (tail s)



data Token =  PLUS | MINUS | MULT | DIV | MOD | LESSEQ | GREATEREQ | LESS | GREATER | EQUAL | NEQUAL | AND | OR | NOT | INT Int | REAL Float | LPAREN | RPAREN | LBRACE | RBRACE | COMMA | ID String | RETURN | VAR | VAL | ASSIGN | STRING String | CHAR Char | BOOL Bool | TINT | TFLOAT | TSTRING | TCHAR | TBOOL | IF | ELSE | WHILE | SEMICOLON
                deriving (Show)
}
