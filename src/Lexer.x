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
[0-9]+"."[0-9]+ {\s -> REAL (read s)}
\"[a-zA-Z0-9_ ]+\" {\s -> STRING s}
\'[a-zA-Z0-9_]\' {\s -> CHAR (read s)}
true|false {\s -> BOOL (read s)}

-- flow control
if {\s -> IF}
else {\s -> ELSE}
while {\s -> WHILE}

-- symbols & keywords
[_a-zA-z]+[_a-zA-Z0-9]* {\s -> ID s}
return {\s -> RETURN}
var {\s -> VAR}
val {\s -> VAL}
"=" {\s -> ASSIGNMENT}

-- types
Int {\s -> TINT}
Float {\s -> TFLOAT}
String {\s -> TSTRING}
Char {\s -> TCHAR}
Boolean {\s -> TBOOL}

{

data Token =  PLUS | MINUS | MULT | DIV | MOD | LESSEQ | GREATEREQ | LESS | GREATER | EQUAL | NEQUAL | AND | OR | NOT | INT Int | REAL Float | LPAREN | RPAREN | LBRACE | RBRACE | COMMA | ID name | RETURN | VAR | VAL | ASSIGNMENT | STRING String | CHAR Char | BOOL Bool | TINT | TFLOAT | TSTRING | TCHAR | TBOOL

}
