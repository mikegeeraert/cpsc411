

module AbsMplus where

-- Haskell module generated by the BNF converter

data M_prog = M_prog ([M_decl],[M_stmt]) 
            deriving(Eq, Show)

data M_decl = M_var (String, [M_expr], M_type) 
            | M_fun (String, [(String,Int,M_type)], M_type, [M_decl], [M_stmt])
            deriving(Eq, Show)

data M_stmt = M_ass (String, [M_expr], M_expr)
            | M_while (M_expr, M_stmt)
            | M_cond (M_expr, M_stmt, M_stmt) 
            | M_read (String, [M_expr])
            | M_print M_expr
            | M_return M_expr
            | M_block ([M_decl], [M_stmt])
            deriving(Eq, Show)

data M_type = M_int | M_bool | M_real 
            deriving(Eq, Show)

data M_expr = M_ival Integer
            | M_rval Float
            | M_bval Bool
            | M_size (String,Int)
            | M_id (String,[M_expr])
            | M_app (M_operation,[M_expr])
            deriving(Eq, Show)

data M_operation = M_fn String | M_add | M_mul | M_sub | M_div | M_neg
                 | M_lt | M_le | M_gt | M_ge | M_eq | M_not | M_and | M_or
                 | M_float | M_floor | M_ceil
                 deriving(Eq, Show)

{-
newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Start = START Block
  deriving (Eq, Ord, Show, Read)

data Block = BProg Declarations Program_Body
  deriving (Eq, Ord, Show, Read)

data Declarations = DSemi Declaration Declarations | DEmpty
  deriving (Eq, Ord, Show, Read)

data Declaration = DVar Var_Declaration | DFun Fun_Declaration
  deriving (Eq, Ord, Show, Read)

data Var_Declaration = VD Ident Array_Dimensions Type
  deriving (Eq, Ord, Show, Read)

data Type = TInt | TReal | TBool
  deriving (Eq, Ord, Show, Read)

data Array_Dimensions = ADExpr Expr Array_Dimensions | ADEmpty
  deriving (Eq, Ord, Show, Read)

data Fun_Declaration = FDDec Ident Param_List Type Fun_Block
  deriving (Eq, Ord, Show, Read)

data Fun_Block = FB Declarations Fun_Body
  deriving (Eq, Ord, Show, Read)

data Param_List = PL Parameters
  deriving (Eq, Ord, Show, Read)

data Parameters
    = PParams Basic_Declaration More_Parameters | PEmpty
  deriving (Eq, Ord, Show, Read)

data More_Parameters
    = MPMore Basic_Declaration More_Parameters | MPEmpty
  deriving (Eq, Ord, Show, Read)

data Basic_Declaration = BDBasic Ident Basic_Array_Dimensions Type
  deriving (Eq, Ord, Show, Read)

data Basic_Array_Dimensions = BAD Basic_Array_Dimensions | BADEmpty
  deriving (Eq, Ord, Show, Read)

data Program_Body = PBBody Prog_Stmts
  deriving (Eq, Ord, Show, Read)

data Fun_Body = FBBody Prog_Stmts Expr
  deriving (Eq, Ord, Show, Read)

data Prog_Stmts = PSSemi Prog_Stmt Prog_Stmts | PSEmpty
  deriving (Eq, Ord, Show, Read)

data Prog_Stmt
    = PSITE Expr Prog_Stmt Prog_Stmt
    | PSWhile Expr Prog_Stmt
    | PSRead Identifier
    | PSAssign Identifier Expr
    | PSPrint Expr
    | PSCPar Block
  deriving (Eq, Ord, Show, Read)

data Identifier = ID Ident Array_Dimensions
  deriving (Eq, Ord, Show, Read)

data Expr = EOr Expr Bint_Term | EBint Bint_Term
  deriving (Eq, Ord, Show, Read)

data Bint_Term = BTAnd Bint_Term Bint_Factor | BTFactor Bint_Factor
  deriving (Eq, Ord, Show, Read)

data Bint_Factor
    = BFNot Bint_Factor
    | BFComp Int_Expr Compare_Op Int_Expr
    | BFExpr Int_Expr
  deriving (Eq, Ord, Show, Read)

data Compare_Op = COEq | COLt | COGt | COLe | COGe
  deriving (Eq, Ord, Show, Read)

data Int_Expr = IEAddop Int_Expr Addop Int_Term | IETerm Int_Term
  deriving (Eq, Ord, Show, Read)

data Addop = AAdd | ASub
  deriving (Eq, Ord, Show, Read)

data Int_Term
    = ITMul Int_Term Mulop Int_Factor | ITFactor Int_Factor
  deriving (Eq, Ord, Show, Read)

data Mulop = MMul | MDiv
  deriving (Eq, Ord, Show, Read)

data Int_Factor
    = IFPar Expr
    | IFArray Basic_Array_Dimensions
    | IFFLoat Expr
    | IFFloor Expr
    | IFCeil Expr
    | IFModList Ident Modifier_List
    | IFInt Integer
    | IFReal Double
    | IFTrue
    | IFFalse
    | IFNeg Int_Factor
  deriving (Eq, Ord, Show, Read)

data Modifier_List = MLArgs Arguments | MLArray Array_Dimensions
  deriving (Eq, Ord, Show, Read)

data Arguments = AExpr Expr More_Arguments | AEmpty
  deriving (Eq, Ord, Show, Read)

data More_Arguments = MAComma Expr More_Arguments | MAEmpty
  deriving (Eq, Ord, Show, Read)
-}