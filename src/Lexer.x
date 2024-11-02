{
module Lexer where
}

%wrapper "basic"

tokens :-

-- syntax
$white+    ;
\( {\s -> LPAREN}
\) {\s -> RPAREN}
\{ {\s -> LBRACE}
\} {\s -> RBRACE}

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

-- values
[0-9]+ {\s -> INT (read s)}
[0-9]+"."[0-9]+ {\s -> REAL (read s)}

-- flow control
if {\s -> IF}
else {\s -> ELSE}
while {\s -> WHILE}
--bool values

-- symbols
[_a-zA-z]+[_a-zA-Z0-9]* {\s -> ID s}

{

data Token =  PLUS | MINUS | MULT | DIV | MOD | LESSEQ | GREATEREQ | LESS | GREATER | EQUAL | NEQUAL | AND | OR | INT Int | REAL Float | LPAREN | RPAREN | LBRACE | RBRACE | ID name

}
