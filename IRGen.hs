module IRGen where

import AST
import IR
import SymbolTable
import SymbolTypes

get_decls :: (Int, ST) -> [M_decl] -> (Int, ST)
get_decls (int, st) [] = (int, st)
get_decls (int, st) (M_var(str, exprs, varType):rest) = case (insert int st (VARIABLE(str, varType, (length exprs)))) of
                                                        (int, st) -> get_decls (int, st) rest
get_decls (int, st) (M_fun(str, args, returnType, decls, stmts):rest) = case insert int st (FUNCTION(str, parseArgs args, returnType)) of
                                                        (int, st) -> case get_decls (int, st) rest of
                                                                    (int, st) -> case new_scope (L_FUN returnType) st of
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


start:: M_prog -> (Int, ST)
start (M_prog(decls, stmts)) = get_decls (1, new_scope L_PROG empty) decls 


--------------------------------------------------------------------------------------------------------------------------------------------------
-- prog_analysis :: M_prog -> Either String I_prog
-- prog_analysis (M_prog(decls, stmts)) = do
--                                         prog_st <- new_scope L_PROG empty
--                                         (int, just_var_st) <- get_var_decls (0, prog_st) decls
--                                         (int',scope_st) <- get_fun_decls (int, just_var_st) decls
--                                         putStrLn $ "\n[Top Level Symbol Table]\n\n" -- for debugging
--                                         putStrLn $ show (scope_st) -- for debugging
--                                         result <- case check_stmts scope_st stmts of 
--                                                     Left emsg -> Left emsg 
--                                                     Right iStmts -> case fun_analysis (int', scope_st) decls of
--                                                                     Left emsg -> Left emsg
--                                                                     Right iFbodys -> Right build_iprog (int', scope_st) iFbodys iStmts
--                                         return result 

prog_analysis :: M_prog -> IO ()
prog_analysis (M_prog(decls, stmts)) = do
    let prog_st = new_scope L_PROG empty
        (int, just_var_st) = get_var_decls (0, prog_st) decls
        (int', scope_st) = get_fun_decls (int, just_var_st) decls
    putStrLn $ "\n[Top Level Symbol Table]\n\n" -- for debugging
    putStrLn $ show (scope_st) -- for debugging
    let result = case check_stmts scope_st stmts [] of 
                    Left emsg -> Left emsg
                    Right iStmts -> case fun_analysis (int', scope_st) decls of
                                        Right iFbodys -> Right (build_iprog (int', scope_st) iFbodys iStmts)
                                        Left emsg -> Left emsg
    putStrLn $ "\n[IR]\n\n"
    putStrLn $ (show result)


-- TODO
build_iprog :: (Int, ST) -> [I_fbody] -> [I_stmt] -> I_prog
build_iprog (int, st) iFbodys iStmts = I_prog(iFbodys, get_num_local_vars st, dims decls, iStmts) 
    where 
        dims :: [(String,SYM_VALUE)] -> [(Int,[I_expr])]
        dims [] = []
        dims (decl:rest) = (dim decl):(dims rest)

        dim :: (String, SYM_VALUE) -> []

fun_analysis :: (Int, ST) -> [M_decl] -> Either String [I_fbody]
fun_analysis (int, st) decls = Right []
-- fun_analysis (int, st) [] = Right []
-- fun_analysis (int, st) (M_fun(stuff)) = iStmt where
--     iStmt ::  Either String [I_fbody]
--     iStmt = new_scope 



get_var_decls :: (Int, ST) -> [M_decl] -> (Int, ST)
get_var_decls (int, st) [] = (int, st)
get_var_decls (int, st) (M_var(str, exprs, varType):rest) = case (insert int st (VARIABLE(str, varType, (length exprs)))) of
                                                        (int, st) -> get_var_decls (int, st) rest
get_var_decls (int, st) (decl:rest) = get_var_decls (int,st) rest

get_fun_decls :: (Int, ST) -> [M_decl] -> (Int, ST)
get_fun_decls (int,st) [] = (int, st)
get_fun_decls (int, st) (M_fun(str, args, returnType, decls, stmts):rest) = case insert int st (FUNCTION(str, parseArgs args, returnType)) of
                                                                             (int, st) -> get_fun_decls (int, st) rest
    where
        parseArgs :: [(String, Int, M_type)] -> [(M_type, Int)]
        parseArgs ((_, int, argType):xs) = (argType, int):(parseArgs xs) 
        parseArgs [] = []
get_fun_decls (int, st) (decl:rest) = get_fun_decls (int,st) rest

check_stmts:: ST -> [M_stmt] -> [I_stmt] -> Either String [I_stmt]
check_stmts st [] iStmts = Right iStmts
check_stmts st (stmt:rest) iStmts = case check_stmt st stmt of
                            Right iStmt -> check_stmts st rest (iStmt:iStmts)
                            Left emsg -> Left emsg

check_stmt:: ST -> M_stmt -> Either String I_stmt
check_stmt st (M_ass(str, exprs, expr)) = case check_ass_stmt st (str, exprs, expr) of
                                            Right iStmt -> Right iStmt
                                            Left emsg -> Left emsg
check_stmt st (M_while(expr, stmt)) = case check_while_stmt st (expr, stmt) of
                                        Right iStmt -> Right iStmt
                                        Left emsg -> Left emsg
check_stmt st (M_cond(expr, stmt1, stmt2)) = case check_cond_stmt st (expr, stmt1, stmt2) of
                                                Right iStmt -> Right iStmt
                                                Left emsg -> Left emsg
check_stmt st (M_read(str, exprs)) = case check_read_stmt st (str, exprs) of
                                            Right iStmt -> Right iStmt
                                            Left emsg -> Left emsg
check_stmt st (M_print(expr)) = case check_print_stmt st expr of
                                        Right iStmt -> Right iStmt 
                                        Left emsg -> Left emsg
check_stmt st (M_return(expr)) = case check_return_stmt st expr of
                                        Right iStmt -> Right iStmt 
                                        Left emsg -> Left emsg
check_stmt st (M_block(decls, stmts)) = case check_block_stmt st (decls, stmts) of
                                            Right iStmt -> Right iStmt 
                                            Left emsg -> Left emsg                

check_ass_stmt :: ST -> (String, [M_expr], M_expr) -> Either String I_stmt
check_ass_stmt = undefined

check_while_stmt :: ST -> (M_expr, M_stmt) -> Either String I_stmt
check_while_stmt = undefined

check_cond_stmt :: ST -> (M_expr, M_stmt, M_stmt) -> Either String I_stmt
check_cond_stmt = undefined

check_read_stmt :: ST -> (String, [M_expr]) -> Either String I_stmt
check_read_stmt = undefined

check_print_stmt :: ST -> M_expr -> Either String I_stmt
check_print_stmt = undefined

check_return_stmt :: ST -> M_expr -> Either String I_stmt
check_return_stmt = undefined

check_block_stmt :: ST -> ([M_decl], [M_stmt]) -> Either String I_stmt
check_block_stmt = undefined



