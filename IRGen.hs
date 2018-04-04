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
    let result = case check_stmts scope_st stmts of 
                    Left emsg -> Left emsg
                    Right iStmts -> case fun_analysis (int', scope_st) decls of
                                        Left emsg -> Left emsg
                                        Right iFbodys -> Right (build_iprog (int', scope_st) iFbodys iStmts)
                                        
    putStrLn $ "\n[IR]\n\n"
    putStrLn $ (show result)


-- TODO
build_iprog :: (Int, ST) -> [I_fbody] -> [I_stmt] -> I_prog
build_iprog (int, st) iFbodys iStmts = IPROG(iFbodys, get_num_local_vars st, get_array_specifications (get_i_descs st), iStmts) 
    where 
        get_array_specifications :: [SYM_I_DESC] -> [(Int,[I_expr])]
        get_array_specifications [] = []
        get_array_specifications (decl:rest) = case (get_array_spec decl) of
                                                Just array_spec -> array_spec:(get_array_specifications rest)
                                                Nothing -> get_array_specifications rest

get_array_spec :: SYM_I_DESC -> Maybe (Int, [I_expr])
get_array_spec (I_VARIABLE(level, offset, M_int, dim)) = Just (offset, replicate dim (IINT 0))
get_array_spec (I_VARIABLE(level, offset, M_bool, dim)) = Just (offset, replicate dim (IBOOL False))
get_array_spec (I_VARIABLE(level, offset, M_real, dim)) = Just (offset, replicate dim (IREAL 0.0))
get_array_spec _ = Nothing


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

check_stmts:: ST -> [M_stmt] -> Either String [I_stmt]
check_stmts st [] = Right []
check_stmts st (stmt:rest) = case check_stmt st stmt of
                            Left emsg -> Left emsg
                            Right iStmt -> case check_stmts st rest of
                                            Left emsg -> Left emsg
                                            Right iStmts -> Right (iStmt:iStmts)            

check_ass_stmt :: ST -> (String, [M_expr], M_expr) -> Either String I_stmt
check_ass_stmt st (var, [], expr) = case lookup_decl st var of
                                    I_FUNCTION(_,_,_,_) -> Left ("Statment Error: Cannot assign value to function " ++ var) 
                                    I_VARIABLE(level, offset, varType, dim) -> case check_expr st expr of
                                                                                Left emsg -> Left ("In ASSIGNMENT " ++ show ((var, expr)) ++ " - " ++ emsg)
                                                                                Right (iExpr, exprType) -> case exprType == varType of 
                                                                                                            False -> Left ("Statement Error: Missmatched types in assignment of type " ++ show (exprType) ++ " to variable " ++ var ++ " of type " ++ show (varType))
                                                                                                            True -> Right (IASS((level,offset,[],iExpr)))                                                                                                              
check_while_stmt :: ST -> (M_expr, M_stmt) -> Either String I_stmt
check_while_stmt st (expr, stmt) = case check_expr st expr of
                                    Left emsg -> Left ("In WHILE statement " ++ show ((expr, stmt)) ++ " - " ++ emsg)
                                    Right (iExpr, mType) -> case mType of
                                                            M_int -> Left ("Statement Error: Expression in (" ++ show(expr) ++ ") is of type int, and must be bool")
                                                            M_real -> Left ("Statement Error: Expression in (" ++ show(expr) ++ ") is of type real, and must be bool")
                                                            M_bool -> case check_stmt st stmt of 
                                                                        Left emsg -> Left ("In WHILE statement " ++ show ((expr, stmt)) ++ " - " ++ emsg)
                                                                        Right iStmt -> Right (IWHILE(iExpr, iStmt))                                                                        
check_cond_stmt :: ST -> (M_expr, M_stmt, M_stmt) -> Either String I_stmt
check_cond_stmt st (expr, stmt1, stmt2) = case check_expr st expr of
                                    Left emsg -> Left ("In CONDITIONAL statement " ++ show ((expr, stmt1, stmt2)) ++ " - " ++ emsg)
                                    Right (iExpr, mType) -> case mType of
                                                            M_int -> Left ("Statement Error: Expression in (" ++ show(expr) ++ ") is of type int, and must be bool")
                                                            M_real -> Left ("Statement Error: Expression in (" ++ show(expr) ++ ") is of type real, and must be bool")
                                                            M_bool -> case check_stmts st (stmt1:stmt2:[]) of
                                                                            Left emsg -> Left ("In CONDITIONAL statement " ++ show ((expr, stmt1, stmt2)) ++ " - " ++ emsg)
                                                                            Right (iStmt1:iStmt2:[]) -> Right (ICOND(iExpr, iStmt1, iStmt2))
check_read_stmt :: ST -> (String, [M_expr]) -> Either String I_stmt
check_read_stmt = undefined

check_print_stmt :: ST -> M_expr -> Either String I_stmt
check_print_stmt = undefined

check_return_stmt :: ST -> M_expr -> Either String I_stmt
check_return_stmt = undefined

check_block_stmt :: ST -> ([M_decl], [M_stmt]) -> Either String I_stmt
check_block_stmt = undefined

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

-- data I_opn = ICALL      (String,Int)
--            | IADD_F | IMUL_F | ISUB_F | IDIV_F | INEG_F
--            | ILT_F  | ILE_F  | IGT_F  | IGE_F  | IEQ_F   -- operations for floats
--            | IADD | IMUL | ISUB | IDIV | INEG
--            | ILT  | ILE  | IGT  | IGE  | IEQ 
--            | INOT | IAND | IOR | IFLOAT | ICEIL |IFLOOR
-- data M_operation = M_fn String | M_add | M_mul | M_sub | M_div | M_neg
--                  | M_lt | M_le | M_gt | M_ge | M_eq | M_not | M_and | M_or
--                  | M_float | M_floor | M_ceil
--                  deriving(Eq, Show, Generic)
