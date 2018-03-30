module SymbolTypes where   

import AST

data ScopeType = L_PROG | L_FUN M_type | L_BLK | L_CASE 
              deriving (Show)

data SYM_DESC = ARGUMENT (String, M_type, Int)
              | FUNCTION (String, [(M_type, Int)], M_type)
              | VARIABLE (String, M_type, Int)
              deriving (Show)

data SYM_I_DESC = I_VARIABLE (Int,Int,M_type,Int)
                | I_FUNCTION (Int,String,[(M_type,Int)],M_type)
                deriving (Show)