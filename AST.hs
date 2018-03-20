{-# LANGUAGE DeriveGeneric #-}


module AST where

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

data M_prog = M_prog ([M_decl],[M_stmt]) 
            deriving(Eq, Show, Generic)

data M_decl = M_var (String, [M_expr], M_type) 
            | M_fun (String, [(String,Int,M_type)], M_type, [M_decl], [M_stmt])
            deriving(Eq, Show, Generic)

data M_stmt = M_ass (String, [M_expr], M_expr)
            | M_while (M_expr, M_stmt)
            | M_cond (M_expr, M_stmt, M_stmt) 
            | M_read (String, [M_expr])
            | M_print M_expr
            | M_return M_expr
            | M_block ([M_decl], [M_stmt])
            deriving(Eq, Show, Generic)

data M_type = M_int | M_bool | M_real 
            deriving(Eq, Show, Generic)

data M_expr = M_ival Integer
            | M_rval Float
            | M_bval Bool
            | M_size (String,Int)
            | M_id (String,[M_expr])
            | M_app (M_operation,[M_expr])
            deriving(Eq, Show, Generic)

data M_operation = M_fn String | M_add | M_mul | M_sub | M_div | M_neg
                 | M_lt | M_le | M_gt | M_ge | M_eq | M_not | M_and | M_or
                 | M_float | M_floor | M_ceil
                 deriving(Eq, Show, Generic)


astStyle :: Style
astStyle = Style {mode = PageMode, lineLength = 100, ribbonsPerLine = 1}

prettyPrint :: M_prog -> IO()
prettyPrint i  = ppStyle astStyle i

instance Out M_prog where
  doc (M_prog (decls, stmts)) = parens $ text "M_prog:" $$ nest 1 (doc decls) $$ nest 1 (doc stmts)
  docPrec _ = doc
instance Out M_decl where
  doc (M_var (varId, exprs, mType)) = parens $ text "M_var:" <+> text varId $$ nest 1 (doc exprs) $$ nest 1 (doc mType)
  doc (M_fun (funId, args, returnType, decls, stmts)) = parens $ text "M_fun:" <+> text funId $$ nest 1 (doc args ) $$ nest 1 (doc returnType) $$ nest 1 (doc decls) $$ nest 1 (doc stmts) 
  docPrec _ = doc
instance Out M_stmt where 
  doc (M_ass (varId, exprs, expr)) = parens $ text "M_ass:" <+> text varId $$ nest 1 (doc exprs) $$ nest 1 (doc expr)
  doc (M_while (expr, stmt)) = parens $ text "M_while:" $$ nest 1 (doc expr) $$ nest 1 (doc stmt)
  doc (M_cond (expr, stmt1, stmt2)) = parens $ text "M_cond:" $$ nest 1 (doc expr) $$ nest 1 (doc stmt1) $$ nest 1(doc stmt2)
  doc (M_read (varId, exprs)) = parens $ text "M_read:" <+> text varId $$ nest 1 (doc exprs)
  doc (M_print expr) = parens $ text "M_print:" $$ nest 1 (doc expr)
  doc (M_return expr) = parens $ text "M_return:" $$ nest 1 (doc expr)
  doc (M_block (decls, exprs)) = parens $ text "M_block:" $$ nest 1 (doc decls) $$ nest 1 (doc exprs)

instance Out M_expr where
  doc (M_ival mInt) = parens $ text "M_ival:" <+> text (show mInt)
  doc (M_rval mReal) = parens $ text "M_rval:" <+> text (show mReal) 
  doc (M_bval mBool) = parens $ text "M_bval:" <+> text (show mBool)
  doc (M_size (varId, mInt)) = parens $ text "M_size:" <+> text varId $$ nest 1 (text (show mInt))
  doc (M_id (varId, exprs)) = parens $ text "M_id:" <+> text varId $$ nest 1 (doc exprs)
  doc (M_app (op, exprs)) = parens $ text "M_app:" $$ nest 1 (doc op) $$ nest 1 (doc exprs)

instance Out M_type
instance Out M_operation
