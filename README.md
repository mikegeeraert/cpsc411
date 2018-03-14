# Assignment 3: Lexer and Parser

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