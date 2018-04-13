module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)
import Data.Text
import Text.PrettyPrint.GenericPretty


import LexMplus
import ParMplus
import SkelMplus
import PrintMplus
import AbsMplus
import AST (prettyPrint)
import IRGen (prog_analysis)
import IR (prettyPrintIR)
import StackGen




import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun Start -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun Start -> String -> IO ()
run v p s  = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          putStrV v $ "\n[Intermediate Representaion]\n\n"
                          case prog_analysis (transStart tree) of
                            Left emsg -> putStrV v $ emsg
                            Right iProg -> do 
                                           putStrV v (codeGeneration iProg)
                                           writeFile "Output.am" (codeGeneration iProg)
                          exitSuccess


showTree :: Int -> Start -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstrac Syntax Tree]\n\n"
      prettyPrint (transStart tree)

usage :: IO ()
usage = do
  putStrLn $ Prelude.unlines
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