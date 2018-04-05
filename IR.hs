{-# LANGUAGE DeriveGeneric #-}

module IR where

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

data I_prog  = IPROG    ([I_fbody],Int,[(Int,[I_expr])],[I_stmt]) deriving (Show, Generic)
    -- a program node consists of 
    --   (a) the list of functions declared
    --   (b) the number of local variables
    --   (c) a list of array specifications (<offset>,<list of bounds>)
    --   (d) the body: a list of statements
data I_fbody = IFUN     (String,[I_fbody],Int,Int,[(Int,[I_expr])],[I_stmt]) deriving (Show, Generic)
    -- a function node consists of 
    --   (a) the label given to the function
    --   (b) the list of local functions declared
    --   (c) the number of local variables
    --   (d) the number of arguments
    --   (c) a list of array specifications (<offset>,<list of bounds>)
    --   (d) the body: a list of statements
data I_stmt = IASS      (Int,Int,[I_expr],I_expr)
                 -- and assignment has argments (<level>,<offset>,<array indices>,expr)
            | IWHILE    (I_expr,I_stmt)
            | ICOND     (I_expr,I_stmt,I_stmt)
            | IREAD_F   (Int,Int,[I_expr])
            | IREAD_I   (Int,Int,[I_expr])
            | IREAD_B   (Int,Int,[I_expr])
            | IPRINT_F  I_expr
            | IPRINT_I  I_expr
            | IPRINT_B  I_expr
            | IRETURN   I_expr
            | IBLOCK    ([I_fbody],Int,[(Int,[I_expr])],[I_stmt])
            deriving (Show, Generic)
            -- a block consists of 
            -- (a) a list of local functions
            -- (b) the number of local varibles declared
            -- (c) a list of array declarations
            -- (d) the body: a lst of statements
data I_expr = IINT      Int
            | IREAL     Float
            | IBOOL     Bool
            | IID       (Int,Int,[I_expr])   
            --  identifier (<level>,<offset>,<array indices>)
            | IAPP      (I_opn,[I_expr])
            | ISIZE     (Int,Int,Int)
            deriving (Show, Generic)
            --   isize(<level>,<offset>,<which dimension>)
            --   level and offset identify which array the last integer 
            --   tells you which dimension you want to look at!!
data I_opn = ICALL      (String,Int)
           | IADD_F | IMUL_F | ISUB_F | IDIV_F | INEG_F
           | ILT_F  | ILE_F  | IGT_F  | IGE_F  | IEQ_F   -- operations for floats
           | IADD | IMUL | ISUB | IDIV | INEG
           | ILT  | ILE  | IGT  | IGE  | IEQ 
           | INOT | IAND | IOR | IFLOAT | ICEIL |IFLOOR
           deriving (Show, Generic)

astStyle :: Style
astStyle = Style {mode = PageMode, lineLength = 100, ribbonsPerLine = 1}

prettyPrintIR :: I_prog -> IO()
prettyPrintIR i  = ppStyle astStyle i

instance Out I_prog where
  doc (IPROG (iFbodys,numLocalVars,arraySpecs,iStmts)) = parens $ text "IPROG:" $$ nest 1 (doc iFbodys) $$ nest 1 (doc numLocalVars) $$ nest 1 (doc arraySpecs) $$ nest 1 (doc iStmts)
  docPrec _ = doc
instance Out I_fbody where
  doc (IFUN (label,iFbodys,numLocalVars,numArgs,arraySpecs,iStmts)) = parens $ text "IFUN:" <+> text label $$ nest 1 (doc iFbodys) $$ nest 1 (doc numLocalVars) $$ nest 1 (doc numArgs) $$ nest 1 (doc arraySpecs) $$ nest 1 (doc iStmts)
  docPrec _ = doc
instance Out I_stmt where 
  doc (IASS (level, offset, arrayIndices, expr)) = parens $ text "IASS: " <+>  text (show(level)) <+> text ", " <+> text (show(offset)) <+> text "," <+> text (show(arrayIndices)) <+> text ", " <+> doc expr
  doc (IWHILE (expr, stmt)) = parens $ text "IWHILE: " <+> (doc expr) $$ nest 1 (doc stmt)
  doc (ICOND (expr, stmt1, stmt2)) = parens $ text "ICOND:" <+> (doc expr) $$ nest 1 (doc stmt1) $$ nest 1(doc stmt2)
  doc (IREAD_F (level, offset, arrayIndices)) = parens $ text "IREAD_F:" <+> text (show(level)) <+> text "," <+> text (show(offset)) <+> text "," <+> text (show(arrayIndices))
  doc (IREAD_I (level, offset, arrayIndices)) = parens $ text "IREAD_I:" <+> text (show(level)) <+> text "," <+> text (show(offset)) <+> text "," <+> text (show(arrayIndices))
  doc (IREAD_B (level, offset, arrayIndices)) = parens $ text "IREAD_B:" <+> text (show(level)) <+> text "," <+> text (show(offset)) <+> text "," <+> text (show(arrayIndices))
  doc (IPRINT_F (expr)) = parens $ text "IPRINT_F: " <+> (doc expr)
  doc (IPRINT_I (expr)) = parens $ text "IPRINT_I: " <+> (doc expr)
  doc (IPRINT_B (expr)) = parens $ text "IPRINT_B: " <+> (doc expr)
  doc (IRETURN (expr)) = parens $ text "IRETURN: " <+> (doc expr)
  doc (IBLOCK (iFbodys, numLocalVars, arraySpecs, iStmts)) = parens $ text "IBLOCK: " $$ nest 1 (doc iFbodys) $$ nest 1 (doc numLocalVars) $$ nest 1 (doc arraySpecs) $$ nest 1 (doc iStmts) 
  docPrec _ = doc

instance Out I_expr where
instance Out I_opn where