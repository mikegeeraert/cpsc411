# Assignment 3: Lexer and Parser

This is the frontend for the M+ compiler. The language specification was directly translated from http://pages.cpsc.ucalgary.ca/~robin/class/411/M+/M+.txt into [mplus.bnfc](https://github.com/mikegeeraert/cpsc411/blob/master/mplus.bnfc)

The language specification in the .bnfc file is then fed into the [BNF Converter](https://github.com/BNFC/bnfc) which builds:
1. [Alex](https://www.haskell.org/alex/) Lexer Generator File
2. [Happy](https://www.haskell.org/happy/) Parser Generator File
3. Several other files which help in the configuration of the compiler and the `make` process (outlined in **Running and Compiling the Compiler**)

## Requirements

 1. Haskell Platform: https://www.haskell.org/downloads#platform
 - May work with a more basic Haskell installation but I haven't personally tested it
 - The Haskell installation you use **must** include Alex and Happy

## Running and Compiling the Compiler:

 1. Download this repo either as a zip or by running: 
 	`git clone https://github.com/mikegeeraert/cpsc411.git`
 	in a directory of your choosing
 	*or*
 	Download the zip and unpack to a directory of your choosing 
 2. Navigate to the top level directory ie -> `youDirectory/cpsc411/`

 3. run `make` to compile the ParMplus.y parser, as well as the LexMplus.x lexer

 4. test the compiler using `./TestMplus *inputfile.txt* *outputfile.txt*`
  - there are many tests in the `cpsc411/tests/` directory which have been downloaded from Dr. Cockett's website