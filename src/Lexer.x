{
module Lexer where
}

%wrapper "basic"

tokens :-

-- syntax
\/\/.*$             ;
\/\*(.|\s)*\?\*\/     ;
\n {\s -> NEWLINE}
\; {\s -> SEMICOLON}
[\ \t]+ ;
\( {\s -> LPAREN}
\) {\s -> RPAREN}
\n*\{ {\s -> LBRACE}
\n*\} {\s -> RBRACE}
"," {\s -> COMMA}

-- types
Int {\s -> TINT}
String {\s -> TSTRING}
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
\"[a-zA-Z0-9_\ ]+\" {\s -> STRING (unquote s)}
true {\s -> BOOL True}
false {\s -> BOOL False}

-- flow control
if {\s -> IF}
else {\s -> ELSE}
while {\s -> WHILE}

-- symbols & keywords
var {\s -> VAR}
fun {\s -> FUN}
return {\s -> RETURN}
[_a-zA-z]+[_a-zA-Z0-9]* {\s -> ID s}
":" {\s -> COLON}
"=" {\s -> ASSIGN}

{
unquote :: String -> String
unquote s = init (tail s)

data Token =  PLUS | MINUS | MULT | DIV | MOD
           | LESSEQ | GREATEREQ | LESS | GREATER | EQUAL | NEQUAL
           | AND | OR | NOT
           | INT Integer | REAL Double | ID String | STRING String | BOOL Bool
           | TINT | TSTRING | TBOOL
           | LPAREN | RPAREN | LBRACE | RBRACE | COMMA | COLON | FUN | ENDOFSTATEMENT | NEWLINE | SEMICOLON
           | RETURN | VAR | ASSIGN
           | IF | ELSE | WHILE
           deriving (Show)
}
