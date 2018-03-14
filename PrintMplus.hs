{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for PrintMplus.
--   Generated by the BNF converter.

module PrintMplus where

import AbsMplus
import Data.Char

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Ident where
  prt _ (Ident i) = doc (showString i)

instance Print Start where
  prt i e = case e of
    START block -> prPrec i 0 (concatD [prt 0 block])

instance Print Block where
  prt i e = case e of
    BProg declarations programbody -> prPrec i 0 (concatD [prt 0 declarations, prt 0 programbody])

instance Print Declarations where
  prt i e = case e of
    DSemi declaration declarations -> prPrec i 0 (concatD [prt 0 declaration, doc (showString ";"), prt 0 declarations])
    DEmpty -> prPrec i 0 (concatD [])

instance Print Declaration where
  prt i e = case e of
    DVar vardeclaration -> prPrec i 0 (concatD [prt 0 vardeclaration])
    DFun fundeclaration -> prPrec i 0 (concatD [prt 0 fundeclaration])

instance Print Var_Declaration where
  prt i e = case e of
    VD id arraydimensions type_ -> prPrec i 0 (concatD [doc (showString "var"), prt 0 id, prt 0 arraydimensions, doc (showString ":"), prt 0 type_])

instance Print Type where
  prt i e = case e of
    TInt -> prPrec i 0 (concatD [doc (showString "int")])
    TReal -> prPrec i 0 (concatD [doc (showString "real")])
    TBool -> prPrec i 0 (concatD [doc (showString "bool")])

instance Print Array_Dimensions where
  prt i e = case e of
    ADExpr expr arraydimensions -> prPrec i 0 (concatD [doc (showString "["), prt 0 expr, doc (showString "]"), prt 0 arraydimensions])
    ADEmpty -> prPrec i 0 (concatD [])

instance Print Fun_Declaration where
  prt i e = case e of
    FDDec id paramlist type_ funblock -> prPrec i 0 (concatD [doc (showString "fun"), prt 0 id, prt 0 paramlist, doc (showString ":"), prt 0 type_, doc (showString "{"), prt 0 funblock, doc (showString "}")])

instance Print Fun_Block where
  prt i e = case e of
    FB declarations funbody -> prPrec i 0 (concatD [prt 0 declarations, prt 0 funbody])

instance Print Param_List where
  prt i e = case e of
    PL parameters -> prPrec i 0 (concatD [doc (showString "("), prt 0 parameters, doc (showString ")")])

instance Print Parameters where
  prt i e = case e of
    PParams basicdeclaration moreparameters -> prPrec i 0 (concatD [prt 0 basicdeclaration, prt 0 moreparameters])
    PEmpty -> prPrec i 0 (concatD [])

instance Print More_Parameters where
  prt i e = case e of
    MPMore basicdeclaration moreparameters -> prPrec i 0 (concatD [doc (showString ","), prt 0 basicdeclaration, prt 0 moreparameters])
    MPEmpty -> prPrec i 0 (concatD [])

instance Print Basic_Declaration where
  prt i e = case e of
    BDBasic id basicarraydimensions type_ -> prPrec i 0 (concatD [prt 0 id, prt 0 basicarraydimensions, doc (showString ":"), prt 0 type_])

instance Print Basic_Array_Dimensions where
  prt i e = case e of
    BAD basicarraydimensions -> prPrec i 0 (concatD [doc (showString "["), doc (showString "]"), prt 0 basicarraydimensions])
    BADEmpty -> prPrec i 0 (concatD [])

instance Print Program_Body where
  prt i e = case e of
    PBBody progstmts -> prPrec i 0 (concatD [doc (showString "begin"), prt 0 progstmts, doc (showString "end")])

instance Print Fun_Body where
  prt i e = case e of
    FBBody progstmts expr -> prPrec i 0 (concatD [doc (showString "begin"), prt 0 progstmts, doc (showString "return"), prt 0 expr, doc (showString ";"), doc (showString "end")])

instance Print Prog_Stmts where
  prt i e = case e of
    PSSemi progstmt progstmts -> prPrec i 0 (concatD [prt 0 progstmt, doc (showString ";"), prt 0 progstmts])
    PSEmpty -> prPrec i 0 (concatD [])

instance Print Prog_Stmt where
  prt i e = case e of
    PSITE expr progstmt1 progstmt2 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 expr, doc (showString "then"), prt 0 progstmt1, doc (showString "else"), prt 0 progstmt2])
    PSWhile expr progstmt -> prPrec i 0 (concatD [doc (showString "while"), prt 0 expr, doc (showString "do"), prt 0 progstmt])
    PSRead identifier -> prPrec i 0 (concatD [doc (showString "read"), prt 0 identifier])
    PSAssign identifier expr -> prPrec i 0 (concatD [prt 0 identifier, doc (showString ":="), prt 0 expr])
    PSPrint expr -> prPrec i 0 (concatD [doc (showString "print"), prt 0 expr])
    PSCPar block -> prPrec i 0 (concatD [doc (showString "{"), prt 0 block, doc (showString "}")])

instance Print Identifier where
  prt i e = case e of
    ID id arraydimensions -> prPrec i 0 (concatD [prt 0 id, prt 0 arraydimensions])

instance Print Expr where
  prt i e = case e of
    EOr expr bintterm -> prPrec i 0 (concatD [prt 0 expr, doc (showString "or"), prt 0 bintterm])
    EBint bintterm -> prPrec i 0 (concatD [prt 0 bintterm])

instance Print Bint_Term where
  prt i e = case e of
    BTAnd bintterm bintfactor -> prPrec i 0 (concatD [prt 0 bintterm, doc (showString "and"), prt 0 bintfactor])
    BTFactor bintfactor -> prPrec i 0 (concatD [prt 0 bintfactor])

instance Print Bint_Factor where
  prt i e = case e of
    BFNot bintfactor -> prPrec i 0 (concatD [doc (showString "not"), prt 0 bintfactor])
    BFComp intexpr1 compareop intexpr2 -> prPrec i 0 (concatD [prt 0 intexpr1, prt 0 compareop, prt 0 intexpr2])
    BFExpr intexpr -> prPrec i 0 (concatD [prt 0 intexpr])

instance Print Compare_Op where
  prt i e = case e of
    COEq -> prPrec i 0 (concatD [doc (showString "=")])
    COLt -> prPrec i 0 (concatD [doc (showString "<")])
    COGt -> prPrec i 0 (concatD [doc (showString ">")])
    COLe -> prPrec i 0 (concatD [doc (showString "=<")])
    COGe -> prPrec i 0 (concatD [doc (showString ">=")])

instance Print Int_Expr where
  prt i e = case e of
    IEAddop intexpr addop intterm -> prPrec i 0 (concatD [prt 0 intexpr, prt 0 addop, prt 0 intterm])
    IETerm intterm -> prPrec i 0 (concatD [prt 0 intterm])

instance Print Addop where
  prt i e = case e of
    AAdd -> prPrec i 0 (concatD [doc (showString "+")])
    ASub -> prPrec i 0 (concatD [doc (showString "-")])

instance Print Int_Term where
  prt i e = case e of
    ITMul intterm mulop intfactor -> prPrec i 0 (concatD [prt 0 intterm, prt 0 mulop, prt 0 intfactor])
    ITFactor intfactor -> prPrec i 0 (concatD [prt 0 intfactor])

instance Print Mulop where
  prt i e = case e of
    MMul -> prPrec i 0 (concatD [doc (showString "*")])
    MDiv -> prPrec i 0 (concatD [doc (showString "/")])

instance Print Int_Factor where
  prt i e = case e of
    IFPar expr -> prPrec i 0 (concatD [doc (showString "("), prt 0 expr, doc (showString ")")])
    IFArray id basicarraydimensions -> prPrec i 0 (concatD [doc (showString "size"), doc (showString "("), prt 0 id, prt 0 basicarraydimensions, doc (showString ")")])
    IFFLoat expr -> prPrec i 0 (concatD [doc (showString "float"), doc (showString "("), prt 0 expr, doc (showString ")")])
    IFFloor expr -> prPrec i 0 (concatD [doc (showString "floor"), doc (showString "("), prt 0 expr, doc (showString ")")])
    IFCeil expr -> prPrec i 0 (concatD [doc (showString "ceil"), doc (showString "("), prt 0 expr, doc (showString ")")])
    IFModList id modifierlist -> prPrec i 0 (concatD [prt 0 id, prt 0 modifierlist])
    IFInt n -> prPrec i 0 (concatD [prt 0 n])
    IFReal d -> prPrec i 0 (concatD [prt 0 d])
    IFTrue -> prPrec i 0 (concatD [doc (showString "true")])
    IFFalse -> prPrec i 0 (concatD [doc (showString "false")])
    IFNeg intfactor -> prPrec i 0 (concatD [doc (showString "-"), prt 0 intfactor])

instance Print Modifier_List where
  prt i e = case e of
    MLArgs arguments -> prPrec i 0 (concatD [doc (showString "("), prt 0 arguments, doc (showString ")")])
    MLArray arraydimensions -> prPrec i 0 (concatD [prt 0 arraydimensions])

instance Print Arguments where
  prt i e = case e of
    AExpr expr morearguments -> prPrec i 0 (concatD [prt 0 expr, prt 0 morearguments])
    AEmpty -> prPrec i 0 (concatD [])

instance Print More_Arguments where
  prt i e = case e of
    MAComma expr morearguments -> prPrec i 0 (concatD [doc (showString ","), prt 0 expr, prt 0 morearguments])
    MAEmpty -> prPrec i 0 (concatD [])

