module StackGen where


import IR

type LabelNumber = Int
type BlockFunctions = String

codeGeneration:: I_prog -> String
codeGeneration (IPROG(funs, varsize, arraySpecs, stmts)) = progCode
    where
        progStartCode = prog_start varsize
        (progStmtCode, n, blkFuns)  = code_stmts 0 stmts
        progFinishCode = prog_finish varsize
        (progFunCode, n') = code_funs n funs
        progCode = progStartCode ++ progStmtCode ++ progFinishCode ++ progFunCode ++ blkFuns

prog_start:: Int -> String
prog_start varsize =  "\t% -- start program --\n" ++
                        "\tLOAD_R %sp\n" ++ 
                        "\tLOAD_R %sp\n" ++ 
                        "\tSTORE_R %fp\n" ++ 
                        "\tALLOC " ++ show varsize ++ "\n"

prog_finish:: Int -> String
prog_finish varsize = "\t% -- end program --\n" ++
                        "\tALLOC -" ++ show varsize ++ "\n" ++ 
                        "\tHALT\n"

code_funs:: LabelNumber -> [I_fbody] -> (String, LabelNumber)
code_funs n [] = ("", n)
code_funs n (fun:rest) = result 
 where
    (funCode, n') = code_fun n fun 
    (funsCode, n'') = code_funs n'' rest
    result = (funCode ++ funsCode, n'')

code_fun:: LabelNumber -> I_fbody -> (String, LabelNumber)
code_fun n (IFUN(label, funs, varsize, argsize, arraySpecs, stmts)) = (funCode, n'')
    where
        funStartCode = fun_start label varsize 
        (funStmtCode, n', blkFuns) = code_stmts n stmts
        funFinishCode = fun_finish argsize varsize
        (nestedFunCode, n'') = code_funs n' funs
        funCode = funStartCode ++ funStmtCode ++ funFinishCode ++ nestedFunCode ++ blkFuns


fun_start:: String -> Int -> String
fun_start label varsize = "\t% -- begin function -- \n" ++
                            label ++ ":" ++ "\tLOAD_R %sp\n" ++ 
                            "\tSTORE_R %fp\n" ++ 
                            "\tALLOC " ++ show varsize ++ "\n" ++ 
                            "\tLOAD_I " ++ show (varsize+2) ++ "\n"

fun_finish:: Int -> Int -> String
fun_finish argsize varsize  =  "\t% -- return from function --\n" ++
                        "\tLOAD_R %fp\n" ++ 
                        "\tSTORE_O " ++ show (-(argsize + 3)) ++ "\n" ++ 
                        "\tLOAD_R %fp\n" ++ 
                        "\tLOAD_O 0\n" ++ 
                        "\tLOAD_R %fp\n" ++ 
                        "\tSTORE_O " ++ show (-(argsize + 2)) ++ "\n" ++
                        "\tLOAD_R %fp \n" ++
                        "\tLOAD_O " ++ show (varsize+1) ++ "\n" ++
                        "\tAPP NEG\n" ++
                        "\tALLOC_S\n" ++
                        "\tSTORE_R %fp\n" ++
                        "\tALLOC " ++ show (-argsize) ++ "\n" ++
                        "\tJUMP_S\n"


code_stmts:: LabelNumber -> [I_stmt]  -> (String, LabelNumber, BlockFunctions)
code_stmts n [] = ("", n, "")
code_stmts n (stmt:rest) = result
    where
        (stmtcode, n', blkfuns) = code_stmt n stmt 
        (stmtscode, n'', blkfunsrest) = code_stmts n' rest
        result = ((stmtcode ++ stmtscode), n'', (blkfuns ++ blkfunsrest))

code_stmt:: LabelNumber -> I_stmt -> (String, LabelNumber, BlockFunctions)
code_stmt n (IASS(level, offset, arraySpecs, expr)) = (code_expr expr ++ 
                                                       code_pointer level ++
                                                       "\tSTORE_O " ++ show offset ++ "\n",
                                                       n,
                                                       "")

code_stmt n (IWHILE(expr, stmt)) = (code, n''', blkfuns)
    where
        (endLabel, n') = (("endwhile_" ++ show n), n+1)
        (guardLabel, n'') = (("while_" ++ show n'), n'+1)
        (stmtCode, n''', blkfuns) = code_stmt n'' stmt
        exprCode = code_expr expr
        code = "\t% -- while loop --\n" ++
               guardLabel ++ ":" ++ exprCode ++
               "\tJUMP_C " ++ endLabel ++ "\n" ++
               stmtCode ++
               "\tJUMP " ++ guardLabel ++ "\n" ++
               endLabel ++ ":\n"
code_stmt n (ICOND(expr, stmt1, stmt2)) = (code, n'''', (blkfuns1 ++ blkfuns2))
    where 
        (elseLabel, n') = (("else_" ++ show n), n+1)
        (endLabel, n'') = (("endif_" ++ show n''), n'+1)
        (stmt1Code, n''', blkfuns1) = code_stmt n'' stmt1
        (stmt2Code, n'''', blkfuns2) = code_stmt n''' stmt2
        exprCode = code_expr expr
        code = "\t% -- conditional statement --\n" ++
               exprCode ++
               "\tJUMP_C " ++ elseLabel ++ "\n" ++
               stmt1Code ++
               "\tJUMP " ++ endLabel ++ "\n" ++
               elseLabel ++ ":" ++ stmt2Code ++
               endLabel ++ ":\n"

code_stmt n (IREAD_F(level, offset, exprs)) = (code, n, "")
    where
        code = "\tREAD_F\n" ++
               code_pointer level ++
               "\tSTORE_O " ++ show offset ++ "\n"

code_stmt n (IREAD_I(level, offset, exprs)) = (code, n, "")
    where 
        code = "\tREAD_I\n" ++
               code_pointer level ++
               "\tSTORE_O " ++ show offset ++ "\n"

code_stmt n (IREAD_B(level ,offset, exprs)) = (code, n, "")
    where 
        code = "\tREAD_B\n" ++
               code_pointer level ++
               "\tSTORE_O " ++ show offset ++ "\n"

code_stmt n (IPRINT_F expr) = (code, n, "")
    where
        code = code_expr expr ++
               "\tPRINT_F\n"

code_stmt n (IPRINT_I expr) = (code, n, "")
    where
        code = code_expr expr ++
               "\tPRINT_I\n"

code_stmt n (IPRINT_B expr) = (code, n, "")
    where
        code = code_expr expr ++
               "\tPRINT_B\n"

code_stmt n (IRETURN expr) = (code, n, "")
    where
        code = code_expr expr

code_stmt n (IBLOCK(funs, varsize, arraySpecs, stmts)) = (code, n'', funsCode)
    where 
        (stmtsCode, n', blkFuns) = code_stmts n stmts
        (funsCode, n'') = code_funs n' funs
        code = block_start varsize arraySpecs ++
               stmtsCode ++
               block_finish varsize  


block_start:: Int -> [(Int, [I_expr])] -> String
block_start varsize arraySpecs = "\tLOAD_R %fp\n" ++
                           "\tALLOC 2\n" ++
                           "\tLOAD_R %sp\n" ++
                           "\tSTORE_R %fp\n" ++
                           "\tALLOC " ++ show varsize ++ "\n" ++
                           "\tLOAD_I " ++ show (varsize+3) ++ "\n"

block_finish:: Int -> String
block_finish varsize = "\tLOAD_R %fp\n" ++
                       "\tLOAD_O " ++ show(varsize+1) ++ "\n" ++
                       "\tAPP NEG\n" ++
                       "\tALLOC_S\n" ++
                       "\tSTORE_R %fp\n"

code_pointer:: Int -> String
code_pointer level = "\tLOAD_R %fp\n" ++ 
                     (concat $ replicate level "\tLOAD_O -2 \n")

code_exprs:: [I_expr] -> String
code_exprs [] = ""
code_exprs (expr:rest) = (code_expr expr ++ code_exprs rest)

code_expr:: I_expr -> String
code_expr (IINT int) = "\tLOAD_I " ++ show int ++ "\n"

code_expr (IREAL float) = "\tLOAD_F " ++ show float ++ "\n"

code_expr (IBOOL bool) = "\tLOAD_B " ++ showAMbool bool ++ "\n"

code_expr (IID (level, offset, arrayIndices)) = code_pointer level ++ 
                                                "\tLOAD_O " ++ show offset ++ "\n"

code_expr (IAPP (ICALL(label, level), exprs)) = code_exprs (reverse exprs) ++
                                                "\tALLOC 1 \n" ++ 
                                                code_pointer level ++ 
                                                "\tLOAD_R %fp\n" ++ 
                                                "\tLOAD_R %cp\n" ++ 
                                                "\tJUMP " ++ label ++ "\n"

code_expr (IAPP (operation, exprs)) = code_exprs exprs ++
                                      "\tAPP " ++ get_op operation ++ "\n"

get_op:: I_opn -> String
get_op IADD = "ADD" 
get_op IMUL = "MUL"  
get_op ISUB = "SUB"  
get_op IDIV = "DIV" 
get_op INEG = "NEG" 
get_op IADD_F = "ADD_F"   
get_op IMUL_F = "MUL_F"   
get_op ISUB_F = "SUB_F"   
get_op IDIV_F = "DIV_F"   
get_op INEG_F = "NEG_F"   
get_op ILT = "LT"    
get_op ILE = "LE"    
get_op IGT = "GT"    
get_op IGE = "GE"    
get_op IEQ = "EQ"   
get_op ILT_F = "LT_F"     
get_op ILE_F = "LE_F"     
get_op IGT_F = "GT_F"     
get_op IGE_F = "GE_F"     
get_op IEQ_F = "EQ_F"     
get_op INOT = "NOT"   
get_op IAND = "AND"   
get_op IOR = "OR"     
get_op IFLOAT = "FLOAT"   
get_op IFLOOR = "FLOOR"   
get_op ICEIL = "CEIL"    

showAMbool:: Bool -> String
showAMbool True = "true"
showAMbool False = "false" 