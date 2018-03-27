module IRGen where

import AST
import SymbolTable
import SymbolTypes

get_decls :: (Int, ST) -> [M_decl] -> ST
get_decls (int, st) [] = st
get_decls (int, st) (M_var(str, exprs, varType):rest) = get_decls ( insert int st (VARIABLE(str, varType, (length exprs))) ) rest  
get_decls (int, st) (M_fun(str, args, returnType, decls, stmts):rest) = get_decls ( insert int st (FUNCTION(str, parseArgs args, returnType)) ) rest where
    parseArgs :: [(String, Int, M_type)] -> [(M_type, Int)]
    parseArgs ((_, int, argType):xs) = (argType, int):(parseArgs xs) 
    parseArgs [] = []


start:: M_prog -> ST
start (M_prog(decls, stmts)) = get_decls (1, new_scope L_PROG []) decls 