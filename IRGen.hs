module IRGen where

import AST
import SymbolTable
import SymbolTypes

get_decls :: (Int, ST) -> [M_decl] -> ST
get_decls (int, st) [] = st
get_decls (int, st) (M_var(str, exprs, varType):rest) = case (insert int st (VARIABLE(str, varType, (length exprs)))) of
                                                        (int, st) -> get_decls (int, st) rest
get_decls (int, st) (M_fun(str, args, returnType, decls, stmts):rest) = case insert int st (FUNCTION(str, parseArgs args, returnType)) of
                                                        (int, st) -> case get_decls (int, st) rest of
                                                                    st -> case new_scope (L_FUN returnType) st of
                                                                        st -> case get_arg_decls (int,st) args of
                                                                            (int, st) -> get_decls (int, st) decls
    where
        parseArgs :: [(String, Int, M_type)] -> [(M_type, Int)]
        parseArgs ((_, int, argType):xs) = (argType, int):(parseArgs xs) 
        parseArgs [] = []


get_arg_decls :: (Int, ST) -> [(String, Int, M_type)] -> (Int, ST)
get_arg_decls (int, st) ((str, argDim, argType):rest) = case insert int st (ARGUMENT(str, argType, argDim)) of
                                                     (int, st) -> get_arg_decls (int, st) rest
get_arg_decls (int, st) [] = (int, st)


start:: M_prog -> ST
start (M_prog(decls, stmts)) = get_decls (1, new_scope L_PROG empty) decls 

