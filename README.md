This simple front-end compiler is capable of compiling a subset of the kotlin(https://kotlinlang.org/) language down to assembly instructions.

## Features
- Basic arithmetic
- Basic logic operations
- Flow control
- IO
- Variable assignment

# Installing & Using

1. Navigate to the src/ directory
2. Run:

`alex Lexer.x && happy Parser.y && ghc Main.hs && ./Main PATH` 

Where PATH is the path to the kotlin file you wish to compile.
This will result in a .asm file, with the same name as the kotlin file, being generated on PATH.

Tested on Linux version 6.11.6 with GHC 9.2.8.
