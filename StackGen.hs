module StackGen where


import IR

type LabelNumber = Int

codeGeneration:: I_prog -> String
codeGeneration (IPROG(funs, varsize, arraySpecs, stmts)) = progCode
    where
        progStartCode = prog_start varsize
        (progStmtCode, n)  = code_stmts 0 stmts
        progFinishCode = prog_finish varsize
        (progFunCode, n') = code_funs n funs
        progCode = progStartCode ++ progStmtCode ++ progFinishCode ++ progFunCode

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
        (funStmtCode, n') = code_stmts n stmts
        funFinishCode = fun_finish argsize varsize
        (nestedFunCode, n'') = code_funs n' funs
        funCode = funStartCode ++ funStmtCode ++ funFinishCode ++ nestedFunCode


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


code_stmts:: LabelNumber -> [I_stmt]  -> (String, LabelNumber)
code_stmts n [] = ("", n)
code_stmts n (stmt:rest) = result
    where
        (stmtcode, n') = code_stmt n stmt 
        (stmtscode, n'') = code_stmts n' rest
        result = ((stmtcode ++ stmtscode), n'')

code_stmt:: LabelNumber -> I_stmt -> (String, LabelNumber)
code_stmt n (IASS(level, offset, arraySpecs, expr)) = (code_expr expr ++ 
                                                       code_pointer level ++
                                                       "\tSTORE_O" ++ show offset ++ "\n",
                                                       n)

code_stmt n (IWHILE(expr, stmt)) = (code, n''')
    where
        (endLabel, n') = (("endwhile_" ++ show n), n+1)
        (guardLabel, n'') = (("while_" ++ show n'), n'+1)
        (stmtCode, n''') = code_stmt n'' stmt
        exprCode = code_expr expr
        code = "\t% -- while loop --\n" ++
               guardLabel ++ ":" ++ exprCode ++
               "\tJUMP_C " ++ endLabel ++ "\n" ++
               stmtCode ++
               "\tJUMP " ++ guardLabel ++ "\n" ++
               endLabel ++ ":\n"
code_stmt n (ICOND(expr, stmt1, stmt2)) = (code, n'''')
    where 
        (elseLabel, n') = (("else_" ++ show n), n+1)
        (endLabel, n'') = (("endif_" ++ show n''), n'+1)
        (stmt1Code, n''') = code_stmt n'' stmt1
        (stmt2Code, n'''') = code_stmt n''' stmt2
        exprCode = code_expr expr
        code = "\t% -- conditional statement --\n" ++
               exprCode ++
               "\tJUMP_C " ++ elseLabel ++ "\n" ++
               stmt1Code ++
               "\tJUMP " ++ endLabel ++ "\n" ++
               elseLabel ++ ":" ++ stmt2Code ++
               endLabel ++ ":\n"

code_stmt n (IREAD_F(level, offset, exprs)) = (code, n)
    where
        exprsCode = code_exprs exprs
        code = ""

code_stmt n (IREAD_I(level, offset, exprs)) = ("", n)
code_stmt n (IREAD_B(level ,offset, exprs)) = ("", n)
code_stmt n (IPRINT_F expr) = ("", n)
code_stmt n (IPRINT_I expr) = ("", n)
code_stmt n (IPRINT_B expr) = ("", n)
code_stmt n (IRETURN expr) = ("", n)
code_stmt n (IBLOCK(funs, varsize, arraySepcs, stmts)) = ("", n)

code_pointer:: Int -> String
code_pointer level = "\tLOAD_R %fp\n" ++ 
                     (concat $ replicate level "\tLOAD_O -2 \n")

code_exprs:: [I_expr] -> String
code_exprs [] = ""
code_exprs (expr:rest) = (code_expr expr ++ code_exprs rest)

code_expr:: I_expr -> String
code_expr (IINT(int)) = ""
code_expr (IREAL float) = ""
code_expr (IBOOL bool) = ""
code_expr (IID(level, offset, arrayIndices)) = ""
code_expr (IAPP (operation, exprs)) = ""
code_expr (ISIZE (level ,offset , dimension)) = ""
