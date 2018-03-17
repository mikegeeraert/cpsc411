-- automatically generated by BNF Converter
module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import LexMplus
import ParMplus
import SkelMplus
import PrintMplus
import AbsMplus




import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun Start -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun Start -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          showTree v tree

                          exitSuccess


showTree :: Int -> Start -> IO ()
showTree v tree
 = do
      -- putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      -- putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree
      putStrV v $ "\n[Abstrac Syntax]\n\n" ++ show (transStart tree)
      putStrV v $ "\n[Linearized tree]\n\n" ++ show (transStart tree)

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2 pStart
    "-s":fs -> mapM_ (runFile 0 pStart) fs
    fs -> mapM_ (runFile 2 pStart) fs





