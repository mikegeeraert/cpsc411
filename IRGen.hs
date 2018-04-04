module IRGen where

import AST
import IR
import SymbolTable
import SymbolTypes

--------------------------------------------------------------------------------------------------------------------------------------------------
prog_analysis :: M_prog -> Either String I_prog 
prog_analysis (M_prog(decls, stmts)) =
    let prog_st = new_scope L_PROG empty
        (int, just_var_st) = get_var_decls (0, prog_st) decls
        (int', scope_st) = get_fun_decls (int, just_var_st) decls
        result = case check_stmts (int', scope_st) stmts of 
                    Left emsg -> Left emsg
                    Right (int'', iStmts) -> case traverse_funs (int'', scope_st) decls of
                                        Left emsg -> Left emsg
                                        Right (int''', iFbodys) -> Right (build_iprog (int''', scope_st) iFbodys iStmts)
    in result                                   


-- TODO
build_iprog :: (Int, ST) -> [I_fbody] -> [I_stmt] -> I_prog
build_iprog (int, st) iFbodys iStmts = IPROG(iFbodys, get_num_local_vars st, get_array_specifications (get_i_descs st), iStmts) 


get_array_specifications :: [SYM_I_DESC] -> [(Int,[I_expr])]
get_array_specifications [] = []
get_array_specifications (decl:rest) = case (get_array_spec decl) of
                                        Just array_spec -> array_spec:(get_array_specifications rest)
                                        Nothing -> get_array_specifications rest

get_array_spec :: SYM_I_DESC -> Maybe (Int, [I_expr])
get_array_spec (I_VARIABLE(level, offset, mType, dim)) = case dim > 0 of
                                                         False -> Nothing
                                                         True -> case mType of
                                                                    M_int -> Just (offset, replicate dim (IINT 0))
                                                                    M_real -> Just (offset, replicate dim (IREAL 0.0))
                                                                    M_bool -> Just (offset, replicate dim (IBOOL False))
get_array_spec _ = Nothing


fun_analysis :: (Int, ST) -> M_decl -> Either String (Int, I_fbody)
fun_analysis (int, st) (M_fun(str, args, returnType, decls, stmts)) 
    = case get_arg_decls (int, new_scope (L_FUN returnType) st) args of 
        (int', fun_st) -> case get_var_decls (int', fun_st) decls of
                            (int'', fun_st') -> case get_fun_decls (int'', fun_st') decls of
                                                (int''', full_st) -> case check_stmts (int''', full_st) stmts of 
                                                                        Left emsg -> Left ("In Function " ++ str ++ " - " ++ emsg)
                                                                        Right (int'''', iStmts) -> case traverse_funs (int'''', fun_st) decls of
                                                                                                    Left emsg -> Left emsg
                                                                                                    Right (int5, iFBodys) -> case lookup_decl fun_st str of
                                                                                                                                I_FUNCTION(level, label, arg_types, return_type) -> Right (int5, IFUN(label, iFBodys, get_num_local_vars fun_st, length arg_types, get_array_specifications (get_i_descs fun_st), iStmts))
    where
        get_arg_decls :: (Int, ST) -> [(String, Int, M_type)] -> (Int, ST)
        get_arg_decls (int, st) ((str, argDim, argType):rest) = case insert int st (ARGUMENT(str, argType, argDim)) of
                                                             (int, st) -> get_arg_decls (int, st) rest
        get_arg_decls (int, st) [] = (int, st)

traverse_funs :: (Int, ST) -> [M_decl] -> Either String (Int, [I_fbody])
traverse_funs (int, st) [] = Right (int, [])
traverse_funs (int, st) (M_fun(info):rest) = case fun_analysis (int, st) (M_fun(info)) of
                                                Left emsg -> Left emsg
                                                Right (int', iFbody) -> case traverse_funs (int', st) rest of
                                                                        Left emsg -> Left emsg 
                                                                        Right (int'', iFbodys) -> Right (int'', (iFbody:iFbodys))
traverse_funs (int, st) (decl:rest) = traverse_funs (int, st) rest

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

check_stmts:: (Int, ST) -> [M_stmt] -> Either String (Int, [I_stmt])
check_stmts (int, st) [] = Right (int, [])
check_stmts (int, st) (stmt:rest) = case check_stmt (int,st) stmt of
                            Left emsg -> Left emsg
                            Right (int', iStmt) -> case check_stmts (int',st) rest of
                                            Left emsg -> Left emsg
                                            Right (int'',iStmts) -> Right (int'', (iStmt:iStmts))            

check_ass_stmt :: ST -> (String, [M_expr], M_expr) -> Either String I_stmt
check_ass_stmt st (var, arrayExprs, expr) = case lookup_decl st var of
                                    I_FUNCTION(_,_,_,_) -> Left ("Statment Error: Cannot assign value to function " ++ var) 
                                    I_VARIABLE(level, offset, varType, dim) -> case check_exprs st arrayExprs of
                                                                                Left emsg -> Left ("In ASSIGNMENT " ++ show ((var, expr)) ++ " - " ++ emsg)
                                                                                Right iExprs -> case (length iExprs) <= dim of
                                                                                                False -> Left ("Statement Error: Invalid dimension for " ++ var ++ ". Tried to access dimension " ++ show (length iExprs) ++ " but " ++ var ++ " only has " ++ show (dim) ++ " dimensions")
                                                                                                True -> case check_valid_array_indices iExprs of
                                                                                                        False -> Left ("Statement Error: Invalid type for array dimension expression for var " ++ var ++ ". Must be integers, but are actually" ++ show (map snd iExprs))
                                                                                                        True -> case check_expr st expr of
                                                                                                                Left emsg -> Left ("In ASSIGNMENT " ++ show ((var, expr)) ++ " - " ++ emsg)
                                                                                                                Right (iExpr, exprType) -> case exprType == varType of 
                                                                                                                                            False -> Left ("Statement Error: Missmatched types in assignment of type " ++ show (exprType) ++ " to variable " ++ var ++ " of type " ++ show (varType))
                                                                                                                                            True -> Right (IASS((level,offset,(map fst iExprs),iExpr)))                                                                                                              
check_while_stmt :: (Int, ST) -> (M_expr, M_stmt) -> Either String (Int, I_stmt)
check_while_stmt (int, st) (expr, stmt) = case check_expr st expr of
                                    Left emsg -> Left ("In WHILE statement " ++ show ((expr, stmt)) ++ " - " ++ emsg)
                                    Right (iExpr, mType) -> case mType of
                                                            M_int -> Left ("Statement Error: Expression in (" ++ show(expr) ++ ") is of type int, and must be bool")
                                                            M_real -> Left ("Statement Error: Expression in (" ++ show(expr) ++ ") is of type real, and must be bool")
                                                            M_bool -> case check_stmt (int, st) stmt of 
                                                                        Left emsg -> Left ("In WHILE statement " ++ show ((expr, stmt)) ++ " - " ++ emsg)
                                                                        Right (int', iStmt) -> Right (int', IWHILE(iExpr, iStmt))                                                                        
check_cond_stmt :: (Int, ST) -> (M_expr, M_stmt, M_stmt) -> Either String (Int, I_stmt)
check_cond_stmt (int, st) (expr, stmt1, stmt2) = case check_expr st expr of
                                    Left emsg -> Left ("In CONDITIONAL statement " ++ show ((expr, stmt1, stmt2)) ++ " - " ++ emsg)
                                    Right (iExpr, mType) -> case mType of
                                                            M_int -> Left ("Statement Error: Expression in (" ++ show(expr) ++ ") is of type int, and must be bool")
                                                            M_real -> Left ("Statement Error: Expression in (" ++ show(expr) ++ ") is of type real, and must be bool")
                                                            M_bool -> case check_stmts (int, st) (stmt1:stmt2:[]) of
                                                                            Left emsg -> Left ("In CONDITIONAL statement " ++ show ((expr, stmt1, stmt2)) ++ " - " ++ emsg)
                                                                            Right (int', (iStmt1:iStmt2:[])) -> Right (int', ICOND(iExpr, iStmt1, iStmt2))
check_read_stmt :: ST -> (String, [M_expr]) -> Either String I_stmt
check_read_stmt st (str, exprs) = case lookup_decl st str of 
                                    I_FUNCTION(_,_,_,_) -> Left ("Statement Error: Cannot read value to function " ++ str)
                                    I_VARIABLE(level, offset, varType, dim) -> case check_exprs st exprs of
                                                                                Left emsg -> Left ("In READ statement " ++ show (str, exprs) ++ " - " ++ emsg)
                                                                                Right iExprs -> case (length iExprs) <= dim of
                                                                                                False -> Left ("Statement Error: Invalid dimension for " ++ str ++ ". Tried to access dimension " ++ show (length iExprs) ++ " but " ++ str ++ " only has " ++ show (dim) ++ " dimensions")
                                                                                                True -> case check_valid_array_indices iExprs of
                                                                                                        False -> Left ("Statement Error: Invalid type for array dimension expression for var " ++ str ++ ". Must be integers, but are actually" ++ show (map snd iExprs))
                                                                                                        True -> case varType of 
                                                                                                                M_int -> Right (IREAD_I(level, offset, (map fst iExprs)))
                                                                                                                M_real -> Right (IREAD_F(level, offset, (map fst iExprs)))
                                                                                                                M_bool -> Right (IREAD_B(level, offset, (map fst iExprs)))
check_print_stmt :: ST -> M_expr -> Either String I_stmt
check_print_stmt st expr = case check_expr st expr of
                            Left emsg -> Left ("In PRINT statement " ++ show (expr) ++ " - " ++ emsg)
                            Right (iExpr, mType) -> case mType of
                                                    M_int -> Right (IPRINT_I(iExpr))
                                                    M_real -> Right (IPRINT_F(iExpr))
                                                    M_bool -> Right (IPRINT_B(iExpr))
check_return_stmt :: ST -> M_expr -> Either String I_stmt
check_return_stmt st expr = case check_expr st expr of
                            Left emsg -> Left ("In RETURN statement " ++ show (expr) ++ " - " ++ emsg)
                            Right (iExpr, mType) -> Right (IRETURN(iExpr))
check_block_stmt :: (Int, ST) -> ([M_decl], [M_stmt]) -> Either String (Int, I_stmt)
check_block_stmt (int, st) (decls, stmts) = case get_var_decls (int, (new_scope L_BLK st)) decls of
                                            (int', partial_block_st) -> case get_fun_decls (int', partial_block_st) decls of
                                                                        (int'', block_st) -> case check_stmts (int'', block_st) stmts of
                                                                                                Left emsg -> Left ("In BLOCK statement " ++ show (stmts) ++ " - " ++ emsg)
                                                                                                Right (int''', iStmts) -> case traverse_funs (int''', block_st) decls of
                                                                                                                            Left emsg -> Left ("In BLOCK statement " ++ show (decls) ++ " - " ++ emsg)
                                                                                                                            Right (int'''', iFbodys) -> case delete_scope block_st of
                                                                                                                                                        st -> Right (int'''', IBLOCK(iFbodys, (get_num_local_vars block_st), get_array_specifications (get_i_descs st), iStmts))


check_stmt:: (Int,ST) -> M_stmt -> Either String (Int, I_stmt)
check_stmt (int,st) (M_ass(str, exprs, expr)) = case check_ass_stmt st (str, exprs, expr) of
                                            Right iStmt -> Right (int, iStmt)
                                            Left emsg -> Left emsg
check_stmt (int,st) (M_while(expr, stmt)) = case check_while_stmt (int, st) (expr, stmt) of
                                        Right (int', iStmt) -> Right (int', iStmt)
                                        Left emsg -> Left emsg
check_stmt (int,st) (M_cond(expr, stmt1, stmt2)) = case check_cond_stmt (int, st) (expr, stmt1, stmt2) of
                                                Right (int', iStmt) -> Right (int', iStmt)
                                                Left emsg -> Left emsg
check_stmt (int,st) (M_read(str, exprs)) = case check_read_stmt st (str, exprs) of
                                            Right iStmt -> Right (int, iStmt)
                                            Left emsg -> Left emsg
check_stmt (int,st) (M_print(expr)) = case check_print_stmt st expr of
                                        Right iStmt -> Right (int, iStmt) 
                                        Left emsg -> Left emsg
check_stmt (int,st) (M_return(expr)) = case check_return_stmt st expr of
                                        Right iStmt -> Right (int, iStmt) 
                                        Left emsg -> Left emsg
check_stmt (int,st) (M_block(decls, stmts)) = case check_block_stmt (int,st) (decls, stmts) of
                                            Right (int', iStmt) -> Right (int', iStmt) 
                                            Left emsg -> Left emsg     

check_valid_array_indices:: [(I_expr, M_type)] -> Bool
check_valid_array_indices [] = True
check_valid_array_indices ((iExpr, mType):rest) = case mType == M_int of
                                                    False -> False
                                                    True -> check_valid_array_indices rest

check_exprs:: ST -> [M_expr] -> Either String [(I_expr, M_type)]
check_exprs st [] = Right []
check_exprs st (expr:rest) = case check_expr st expr of
                            Left emsg -> Left emsg
                            Right (iExpr, mType) -> case check_exprs st rest of
                                            Left emsg -> Left emsg
                                            Right iExprs -> Right ((iExpr, mType):iExprs) 

check_expr:: ST -> M_expr -> Either String (I_expr, M_type)
check_expr st (M_ival int) = Right (IINT (fromIntegral int), M_int)
check_expr st (M_rval float) = Right (IREAL float, M_real)
check_expr st (M_bval bool) = Right (IBOOL bool, M_bool)
check_expr st (M_size (str, int)) = case lookup_decl st str of 
                                    I_FUNCTION(_,_,_,_) -> Left ("Expression Error: " ++ str ++ " refers to a function but is used as an array")
                                    I_VARIABLE(level, offset, mType, dim) -> case int <= dim of  
                                                                            True -> Right (ISIZE(level, offset, int), mType)
                                                                            False -> Left ("Expression Error: " ++ show(int) ++ " Not a valid dimension for " ++ str)
check_expr st (M_id(str, exprs)) = case lookup_decl st str of
                                    I_FUNCTION(_,_,_,_) -> Left ("Expression Error: " ++ str ++ " refers to a function but is used as a variable")
                                    I_VARIABLE(level, offset, mType, dim) -> case check_exprs st exprs of 
                                                                                Left emsg -> Left emsg
                                                                                Right exprs -> case expr_types_equal exprs of
                                                                                                Left emsg -> Left ("Expression Error: For array" ++ str ++ ", " ++ emsg)
                                                                                                Right exprType -> case exprType == M_int of
                                                                                                                    True -> Right (IID(level, offset, map fst exprs), mType)
                                                                                                                    False -> Left ("Expression Error: Expressions in the array " ++ str ++ " are not intergers")
                                                                                                where
                                                                                                    expr_types_equal:: [(I_expr, M_type)] -> Either String M_type
                                                                                                    expr_types_equal [] = Right M_int
                                                                                                    expr_types_equal ((_,mType):[]) = Right mType
                                                                                                    expr_types_equal ((_,mType1):(_,mType2):rest) = case mType1 == mType2 of
                                                                                                                                                    True -> expr_types_equal rest
                                                                                                                                                    False -> Left ("Not all expressions in array are integers")
check_expr st (M_app(operation, exprs)) = check_app_expr st (M_app(operation, exprs))

check_app_expr:: ST -> M_expr -> Either String (I_expr, M_type)
check_app_expr st (M_app(M_fn str, exprs)) = case lookup_decl st str of 
                                                I_VARIABLE(_,_,_,_) -> Left ("Expression Error: variable " ++ str ++ " is not a function")
                                                I_FUNCTION(level, label, arg_types, return_type) -> case length exprs == length arg_types of 
                                                                                                    False -> Left ("Expression Error: " ++ show (length exprs) ++ "arguments passed to function " ++ str ++ " when it takes " ++ show (length arg_types) ++ "arguments")
                                                                                                    True -> case check_exprs st exprs of
                                                                                                            Left emsg -> Left emsg
                                                                                                            Right iExprs -> case exprs_match_args iExprs arg_types of
                                                                                                                            False -> Left ("Expression Error: Missmatched types in function call to " ++ str ++ ", types called with are (" ++ show (map snd iExprs) ++ "), but function takes (" ++ show (arg_types) ++ ")")
                                                                                                                            True -> Right (IAPP(ICALL(label, level),(map fst iExprs)), return_type)
                                                                                                                            where 
                                                                                                                                exprs_match_args:: [(I_expr, M_type)] -> [(M_type, Int)] -> Bool
                                                                                                                                exprs_match_args [] [] = True
                                                                                                                                exprs_match_args [] _ = False
                                                                                                                                exprs_match_args _ [] = False
                                                                                                                                exprs_match_args ((_,exprType):exprsRest) ((argType, _):argTypesRest) = case exprType == argType of
                                                                                                                                                                                        True -> exprs_match_args exprsRest argTypesRest
                                                                                                                                                                                        False -> False 
check_app_expr st (M_app(M_add, exprs)) = case check_exprs st exprs of 
                                            Left emsg -> Left emsg 
                                            Right iExprs -> case expr_types_equal (map snd iExprs) of
                                                            Left emsg -> Left ("In add operation - "  ++ emsg)
                                                            Right mType -> case mType of
                                                                            M_bool -> Left("Expression Error: Invalid add operation on bool values")
                                                                            M_int -> Right (IAPP(IADD, map fst iExprs), M_int)
                                                                            M_real -> Right (IAPP(IADD_F, map fst iExprs), M_real)
check_app_expr st (M_app(M_mul, exprs)) = case check_exprs st exprs of 
                                            Left emsg -> Left emsg 
                                            Right iExprs -> case expr_types_equal (map snd iExprs) of
                                                            Left emsg ->  Left ("In multiplication operation - " ++ emsg)
                                                            Right mType -> case mType of
                                                                            M_bool -> Left ("Expression Error: Invalid multiplication on bool values")
                                                                            M_int -> Right (IAPP(IMUL, map fst iExprs), M_int)
                                                                            M_real -> Right (IAPP(IMUL_F, map fst iExprs), M_real)
check_app_expr st (M_app(M_sub, exprs)) = case check_exprs st exprs of 
                                            Left emsg -> Left emsg
                                            Right iExprs -> case expr_types_equal (map snd iExprs) of 
                                                            Left emsg -> Left ("In subtraction operation - " ++ emsg)
                                                            Right mType -> case mType of
                                                                            M_bool -> Left ("Expression Error: Invalid subtraction on bool values")
                                                                            M_int -> Right (IAPP(ISUB, map fst iExprs), M_int)
                                                                            M_real -> Right (IAPP(ISUB_F, map fst iExprs), M_real)            
check_app_expr st (M_app(M_div, exprs)) = case check_exprs st exprs of
                                            Left emsg -> Left emsg
                                            Right iExprs -> case expr_types_equal (map snd iExprs) of
                                                            Left emsg -> Left ("In division operation - " ++ emsg)
                                                            Right mType -> case mType of
                                                                            M_bool -> Left ("Expression Error: Invalid division on bool values")
                                                                            M_int -> Right (IAPP(IDIV, map fst iExprs), M_int)
                                                                            M_real -> Right (IAPP(IDIV_F, map fst iExprs), M_real)
check_app_expr st (M_app(M_neg, exprs)) = case check_exprs st exprs of 
                                            Left emsg -> Left emsg
                                            Right [(iExpr, mType)] -> case mType of
                                                                        M_bool -> Left ("Expression Error: Invalid negation of a bool value")
                                                                        M_int -> Right (IAPP(INEG, [iExpr]), M_int)
                                                                        M_real -> Right (IAPP(INEG_F, [iExpr]), M_real)
check_app_expr st (M_app(M_lt, exprs)) = case check_exprs st exprs of
                                            Left emsg -> Left emsg
                                            Right iExprs -> case expr_types_equal (map snd iExprs) of
                                                            Left emsg -> Left ("In \"less-than\" comparison - " ++ emsg)
                                                            Right mType -> case mType of
                                                                            M_bool -> Left ("Expression Error: Invalid comparison on bool values")
                                                                            M_int -> Right (IAPP(ILT, map fst iExprs), M_bool)
                                                                            M_real -> Right (IAPP(ILT_F, map fst iExprs), M_bool)
check_app_expr st (M_app(M_le, exprs)) = case check_exprs st exprs of
                                            Left emsg -> Left emsg
                                            Right iExprs -> case expr_types_equal (map snd iExprs) of
                                                            Left emsg -> Left ("In \"less-than-equal\" comparison - " ++ emsg)
                                                            Right mType -> case mType of
                                                                            M_bool -> Left ("Expression Error: Invalid comparison on bool values")
                                                                            M_int -> Right (IAPP(ILE, map fst iExprs), M_bool)
                                                                            M_real -> Right (IAPP(ILE_F, map fst iExprs), M_bool)
check_app_expr st (M_app(M_gt, exprs)) = case check_exprs st exprs of
                                            Left emsg -> Left emsg
                                            Right iExprs -> case expr_types_equal (map snd iExprs) of
                                                            Left emsg -> Left ("In \"greater-than\" comparison - " ++ emsg)
                                                            Right mType -> case mType of
                                                                            M_bool -> Left ("Expression Error: Invalid comparison on bool values")
                                                                            M_int -> Right (IAPP(IGT, map fst iExprs), M_bool)
                                                                            M_real -> Right (IAPP(IGT_F, map fst iExprs), M_bool)
check_app_expr st (M_app(M_ge, exprs)) = case check_exprs st exprs of
                                            Left emsg -> Left emsg
                                            Right iExprs -> case expr_types_equal (map snd iExprs) of
                                                            Left emsg -> Left ("In \"greater-than-equal\" comparison - " ++ emsg)
                                                            Right mType -> case mType of
                                                                            M_bool -> Left ("Expression Error: Invalid comparison on bool values")
                                                                            M_int -> Right (IAPP(IGE, map fst iExprs), M_bool)
                                                                            M_real -> Right (IAPP(IGE_F, map fst iExprs), M_bool)
check_app_expr st (M_app(M_eq, exprs)) = case check_exprs st exprs of
                                            Left emsg -> Left emsg
                                            Right iExprs -> case expr_types_equal (map snd iExprs) of
                                                            Left emsg -> Left ("In \"equal\" comparison - " ++ emsg)
                                                            Right mType -> case mType of
                                                                            M_bool -> Left ("Expression Error: Invalid comparison on bool values")
                                                                            M_int -> Right (IAPP(IEQ, map fst iExprs), M_bool)
                                                                            M_real -> Right (IAPP(IEQ_F, map fst iExprs), M_bool)
check_app_expr st (M_app(M_not, exprs)) = case check_exprs st exprs of 
                                            Left emsg -> Left emsg
                                            Right [(iExpr, mType)] -> case mType of
                                                                        M_bool -> Right (IAPP(INOT, [iExpr]), M_bool)
                                                                        M_int -> Left ("Expression Error: Invalid not operation on integer")
                                                                        M_real -> Left ("Expression Error: Invalid not operation on real")
check_app_expr st (M_app(M_and, exprs)) = case check_exprs st exprs of 
                                            Left emsg -> Left emsg
                                            Right iExprs -> case expr_types_equal (map snd iExprs) of
                                                            Left emsg -> Left ("In \"and\" comparison - " ++ emsg)
                                                            Right mType -> case mType of
                                                                            M_bool -> Right (IAPP(IAND, map fst iExprs), M_bool)
                                                                            M_int -> Left ("Expression Error: Invalid \"and\" operation on integer")
                                                                            M_real -> Left ("Expression Error: Invalid \"and\" operation on real")
check_app_expr st (M_app(M_or, exprs)) = case check_exprs st exprs of 
                                            Left emsg -> Left emsg
                                            Right iExprs -> case expr_types_equal (map snd iExprs) of
                                                            Left emsg -> Left ("In \"or\" comparison - " ++ emsg)
                                                            Right mType -> case mType of
                                                                            M_bool -> Right (IAPP(IOR, map fst iExprs), M_bool)
                                                                            M_int -> Left ("Expression Error: Invalid \"or\" operation on integer")
                                                                            M_real -> Left ("Expression Error: Invalid \"or\" operation on real")
check_app_expr st (M_app(M_float, exprs)) = case check_exprs st exprs of
                                            Left emsg -> Left emsg
                                            Right [(iExpr, mType)] -> case mType of
                                                                        M_bool -> Left ("Expression Error: Invalid use of built-in function \"float\" on bool")
                                                                        M_int -> Right (IAPP(IFLOAT, [iExpr]), M_real)
                                                                        M_real -> Left ("Expression Error: Invalid use of built-in function \"float\" on real")
check_app_expr st (M_app(M_floor, exprs)) = case check_exprs st exprs of
                                            Left emsg -> Left emsg
                                            Right [(iExpr, mType)] -> case mType of
                                                                        M_bool -> Left ("Expression Error: Invalid use of built-in function \"floor\" on bool")
                                                                        M_int -> Left ("Expression Error: Invalid use of built-in function \"floor\" on integer")
                                                                        M_real -> Right (IAPP(IFLOOR, [iExpr]), M_int)
check_app_expr st (M_app(M_ceil, exprs)) = case check_exprs st exprs of
                                            Left emsg -> Left emsg
                                            Right [(iExpr, mType)] -> case mType of
                                                                        M_bool -> Left ("Expression Error: Invalid use of built-in function \"ceil\" on bool")
                                                                        M_int -> Left ("Expression Error: Invalid use of built-in function \"ceil\" on integer")
                                                                        M_real -> Right (IAPP(ICEIL, [iExpr]), M_int)
-- Use for checking add, sub, mul, div, lt, le, gt, ge, eq, not, and, or expressions in an M_app
expr_types_equal :: [M_type] -> Either String M_type
expr_types_equal (mType:[]) = Left ("Expression Error: Incomplete operation. Only single expression of type " ++ show(mType) ++ " is given.")
expr_types_equal (mType1:mType2:[]) = case mType1 == mType2 of
                                        False -> Left ("Expression Error: Missmatched types in operation: " ++ show(mType1) ++ " and " ++ show(mType2))
                                        True -> Right mType1
expr_types_equal (mType1:mType2:rest) = Left ("Expression Error: Invalid number of arguments given for operation. " ++ show (mType1:mType2:rest))                                                                                                    
