module SymbolTypes where   

import AST

data ScopeType = L_PROG | L_FUN M_type | L_BLK | L_CASE 

data SYM_VALUE = Var_attr (Int,M_type,Int)
              | Fun_attr (String,[(M_type,Int)],M_type)
              | Con_attr (Int, [M_type], String)
              | Typ_attr [String]
              deriving (Eq, Show)


data SYM_TABLE = Symbol_table (ScopeType, Int,Int,[(String,SYM_VALUE)])

type ST = [SYM_TABLE]