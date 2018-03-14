module SkelMplus where

-- Haskell module generated by the BNF converter

import qualified AbsMplus as A
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> String
transIdent x = case x of
  Ident string -> string

transStart :: Start -> A.M_Prog
transStart x = case x of
  START block -> transBlock block

transBlock :: Block -> ([A.M_decl], [M_stmt])
transBlock x = case x of
  BProg declarations programbody -> (transDeclarations declarations, transProgram_Body programbody)

transDeclarations :: Declarations -> [A.M_decl]
transDeclarations x = case x of
  DSemi declaration declarations -> (transDeclaration declaration : transDeclarations declarations)
  DEmpty -> []

transDeclaration :: Declaration -> A.M_decl
transDeclaration x = case x of
  DVar vardeclaration -> transVar_Declaration vardeclaration
  DFun fundeclaration -> transFun_Declaration fundeclaration

transVar_Declaration :: Var_Declaration -> A.M_decl
transVar_Declaration x = case x of
  VD ident arraydimensions type_ -> A.M_var (ident, transArray_Dimensions arraydimensions, transType type_)

transType :: Type -> A.M_type
transType x = case x of
  TInt -> M_int
  TReal -> M_real
  TBool -> M_bool

transArray_Dimensions :: Array_Dimensions -> [A.M_expr]
transArray_Dimensions x = case x of
  ADExpr expr arraydimensions -> (transExpr expr, transArray_Dimensions arraydimensions)
  ADEmpty -> []

transFun_Declaration :: Fun_Declaration -> A.M_decl
transFun_Declaration x = case x of
  FDDec ident paramlist type_ funblock -> M_fun (ident, transParam_List paramlist, transType type_, fst(transFun_Block funblock), snd(transFun_Block funblock))

transFun_Block :: Fun_Block -> ([A.M_decl], [A.M_stmt])
transFun_Block x = case x of
  FB declarations funbody -> (transDeclarations declarations, transFun_Body funbody)

transParam_List :: Param_List -> [(String, Int, A.M_type)]
transParam_List x = case x of
  PL parameters -> transParameters parameters

transParameters :: Parameters -> [(String, Int, A.M_type)]
transParameters x = case x of
  PParams basicdeclaration moreparameters -> (transBasic_Declaration basicdeclaration : transMore_Parameters moreparameters)
  PEmpty -> failure x

transMore_Parameters :: More_Parameters -> [(String, Int, A.M_type)]
transMore_Parameters x = case x of
  MPMore basicdeclaration moreparameters -> (transBasic_Declaration basicdeclaration : transMore_Parameters moreparameters)
  MPEmpty -> failure x

transBasic_Declaration :: Basic_Declaration -> (String, Int, A.M_type)
transBasic_Declaration x = case x of
  BDBasic ident basicarraydimensions type_ -> (ident, transBasic_Array_Dimensions basicarraydimensions, transType type_)

transBasic_Array_Dimensions :: Basic_Array_Dimensions -> Int
transBasic_Array_Dimensions x = case x of
  BAD basicarraydimensions -> 0 --figure out what this needs to be... should '[][]'' be 2? should '[]' be 1?
  BADEmpty -> 0

transProgram_Body :: Program_Body -> [A.M_stmt]
transProgram_Body x = case x of
  PBBody progstmts -> transProg_Stmts progstmts

transFun_Body :: Fun_Body -> [A.M_stmt]
transFun_Body x = case x of
  FBBody progstmts expr -> transProg_Stmts progstmts ++ [A.M_return (transExpr Expr)] -- Try Dot Notation here

transProg_Stmts :: Prog_Stmts -> [A.M_stmt]
transProg_Stmts x = case x of
  PSSemi progstmt progstmts -> (transProg_Stmt progstmt : transProg_Stmts progstmts)
  PSEmpty -> []

transProg_Stmt :: Prog_Stmt -> A.M_stmt
transProg_Stmt x = case x of
  PSITE expr progstmt1 progstmt2 -> A.M_cond (transExpr expr, transProg_Stmt progstmt1, transProg_Stmt progstmt2)
  PSWhile expr progstmt -> A.M_while (transExpr expr, transProg_Stmt progstmt)
  PSRead identifier -> A.M_read (transIdentifier identifier)
  PSAssign identifier expr -> A.M_ass (fst(transIdentifier identifier), snd(transIdentifier identifier), transExpr expr)
  PSPrint expr -> A.M_print (transExpr expr)
  PSCPar block -> A.M_block (transBlock block)

transIdentifier :: Identifier -> (String, [M_expr])
transIdentifier x = case x of
  ID ident arraydimensions -> (ident, transArray_Dimensions arraydimensions)

transExpr :: Expr -> A.M_expr
transExpr x = case x of
  EOr expr bintterm -> A.M_app (A.M_or, [(transExpr expr), (transBint_Term bintterm)])
  EBint bintterm -> transBint_Term bintterm

transBint_Term :: Bint_Term -> A.M_expr
transBint_Term x = case x of
  BTAnd bintterm bintfactor -> A.M_app (A.M_and, [(transBint_Term bintterm), (transBint_Factor bintfactor)] )
  BTFactor bintfactor -> transBint_Factor bintfactor

transBint_Factor :: Bint_Factor -> A.M_Expr
transBint_Factor x = case x of
  BFNot bintfactor -> A.M_app (A.M_not, [transBint_Factor bintfactor])
  BFComp intexpr1 compareop intexpr2 -> A.M_app (transCompare_Op compareop, [(transInt_Expr intexpr1), (transInt_Expr intexpr2)])
  BFExpr intexpr -> transInt_Expr intexpr

transCompare_Op :: Compare_Op -> A.M_operation
transCompare_Op x = case x of
  COEq -> A.M_eq
  COLt -> A.M_lt
  COGt -> A.M_gt
  COLe -> A.M_le
  COGe -> A.M_ge

transInt_Expr :: Int_Expr -> A.M_expr
transInt_Expr x = case x of
  --Figure out the ordering of term/factor
  IEAddop intexpr addop intterm -> A.M_app((transAddop addop), [(transInt_Expr intexpr), (transInt_Term intterm)])
  IETerm intterm -> transInt_Term intterm

transAddop :: Addop -> A.M_operation
transAddop x = case x of
  AAdd -> A.M_add
  ASub -> A.M_sub

transInt_Term :: Int_Term -> A.M_expr
transInt_Term x = case x of
  --figure out the ordering of term/factor
  ITMul intterm mulop intfactor -> A.M_app((transMulop mulop), [(transInt_Term term), (transInt_Factor intfactor)])
  ITFactor intfactor -> transInt_Factor intfactor

transMulop :: Mulop -> A.M_operation
transMulop x = case x of
  MMul -> A.M_mul
  MDiv -> A.M_div

transInt_Factor :: Int_Factor -> A.M_expr
transInt_Factor x = case x of
  IFPar expr -> failure x
  IFArray basicarraydimensions -> failure x
  IFFLoat expr -> failure x
  IFFloor expr -> failure x
  IFCeil expr -> failure x
  IFModList ident modifierlist -> failure x
  IFInt integer -> failure x
  IFReal double -> failure x
  IFTrue -> failure x
  IFFalse -> failure x
  IFNeg intfactor -> failure x

transModifier_List :: Modifier_List -> Result
transModifier_List x = case x of
  MLArgs arguments -> failure x
  MLArray arraydimensions -> failure x

transArguments :: Arguments -> Result
transArguments x = case x of
  AExpr expr morearguments -> failure x
  AEmpty -> failure x

transMore_Arguments :: More_Arguments -> Result
transMore_Arguments x = case x of
  MAComma expr morearguments -> failure x
  MAEmpty -> failure x

