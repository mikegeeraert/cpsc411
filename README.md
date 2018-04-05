# Assignment 3: Lexer and Parser

This is the frontend for the M+ compiler. The language specification was directly translated from http://pages.cpsc.ucalgary.ca/~robin/class/411/M+/M+.txt into [mplus.bnfc](https://github.com/mikegeeraert/cpsc411/blob/master/mplus.bnfc)

The language specification in the .bnfc file is then fed into the [BNF Converter](https://github.com/BNFC/bnfc) which builds:
1. [Alex](https://www.haskell.org/alex/) Lexer Generator File
2. [Happy](https://www.haskell.org/happy/) Parser Generator File
3. Several other files which help in the configuration of the compiler and the `make` process (outlined in **Running and Compiling the Compiler**)

## Requirements

 1. [Haskell Platform](https://www.haskell.org/downloads#platform)
 - May work with a more basic Haskell installation but I haven't personally tested it
 - The Haskell installation you use **must** include Alex and Happy

 2. [GenericPretty](https://hackage.haskell.org/package/text-generic-pretty) package
 - This library is used for pretty printing the AST to the terminal
 - Install using cabal: 
 	`cabal install GenericPretty`

## Downloading and Making the compiler front end:

 1. Download this repo either as a zip or by running: 
 	`git clone https://github.com/mikegeeraert/cpsc411.git`
 	in a directory of your choosing
 	*or*
 	Download the zip and unpack to a directory of your choosing 
 2. Navigate to the top level directory ie -> `yourDirectory/cpsc411/`

 3. run `make` to compile the ParMplus.y parser, as well as the LexMplus.x lexer

### Assignment 3:
 4. test the lexer and parser using `./TestMplus <<inputfile.txt>>`
  - there are many tests in the `cpsc411/tests/` directory which have been downloaded from Dr. Cockett's website
  - The AST is pretty printed to the screen and written to an output file called "Output.txt" in the same directory

##### Testing Files

1. "test{{number}}.txt" files
- All these files have been pulled off Dr. Cockett's website. All of the tests should succeed the lexing + parsing steps, excpet test15.txt, which contains a syntax error

2. "fib.txt", "sumarray.txt", "exp.txt" files
- These files are also examples of real M+ programs that have been pulled off Dr. Cockett's website. These should all compile

2. "mytest{{number}}.txt" files
- All of these files contain syntax errors and should fail during lexing/parsing. The reasons for the failures are at the top of each test file in a comment. 

### Assignment 4:
5. test semantic analysis using `./TestMplus-IR <<inputfile.txt>>`
- There are 7 of my own tests in the myTests/ directory. 

##### Testing Files

1. test1-test4.txt
- These files all test *successful* semantic analysis of the program. There is a more detailed description in a comment at the top of each file

2. test5-test7.txt
- These files test common *failures* that semantic analysis will give if the code is malformed. They also demonstrate the correct usage of the symbol table and typechecking 
