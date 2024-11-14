{
module Lexer where
}

%wrapper "basic"

tokens :-

-- syntax
\/\/.*$             ;
\/\*(.|\s)*\?\*\/   ;
$white+              ;
[\ \t]+ ;
\( {\s -> LPAREN}
\) {\s -> RPAREN}
\n*\{ {\s -> LBRACE}
\n*\} {\s -> RBRACE}
"," {\s -> COMMA}

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
print {\s -> PRINT}
[_a-zA-z]+[_a-zA-Z0-9]* {\s -> ID s}
"=" {\s -> ASSIGN}

{
unquote :: String -> String
unquote s = init (tail s)

data Token =  PLUS | MINUS | MULT | DIV | MOD
           | LESSEQ | GREATEREQ | LESS | GREATER | EQUAL | NEQUAL
           | AND | OR | NOT
           | INT Integer | ID String | STRING String | BOOL Bool
           | LPAREN | RPAREN | LBRACE | RBRACE | COMMA | FUN
           | RETURN | PRINT | VAR | ASSIGN
           | IF | ELSE | WHILE
           deriving (Show)
}
