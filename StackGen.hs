module StackGen where


import IR



codeGeneration:: I_prog -> String
codeGeneration (IPROG(funs, varsize, arraySpecs, stmts)) = progCode
    where
        progStartCode = prog_start varsize
        progStmtCode  = code_stmts stmts
        progFinishCode = prog_finish varsize
        progFunCode = code_funs funs
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

code_funs:: [I_fbody] -> String
code_funs [] = ""
code_funs (fun:rest) = (code_fun fun ++ code_funs rest)

code_fun:: I_fbody -> String
code_fun (IFUN(label, funs, varsize, argsize, arraySpecs, stmts)) = funCode
    where
        funStartCode = fun_start label varsize 
        funStmtCode = code_stmts stmts
        funFinishCode = fun_finish argsize varsize
        nestedFunCode = code_funs funs
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


code_stmts:: [I_stmt] -> String
code_stmts [] = ""
code_stmts (stmt:rest) = (code_stmt stmt ++ code_stmts rest)

code_stmt:: I_stmt -> String
code_stmt (IASS(level, offset, arraySpecs, expr)) = code_expr expr ++ 
                                                    code_pointer level ++
                                                    "\tSTORE_O" ++ show offset "\n"


code_stmt (IWHILE(expr, stmt)) = ""
code_stmt (ICOND(expr, stmt1, stmt2)) = ""
code_stmt (IREAD_F(level, offset, exprs)) = ""
code_stmt (IREAD_I(level, offset, exprs)) = ""
code_stmt (IREAD_B(level ,offset, exprs)) = ""
code_stmt (IPRINT_F expr) = ""
code_stmt (IPRINT_I expr) = ""
code_stmt (IPRINT_B expr) = ""
code_stmt (IRETURN  expr) = ""
code_stmt (IBLOCK(funs, varsize, arraySepcs, stmts)) = ""

code_pointer:: Int -> String
code_pointer level = "\tLOAD_R %fp\n" ++ 
                     (concat $ replicate lv "\tLOAD_O -2 \n")

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
