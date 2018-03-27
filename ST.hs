module ST where   



data SYM_VALUE = Var_attr (Int,M_type,Int)
              | Fun_attr (String,[(M_type,Int)],M_type)
              | Con_attr (Int, [M_type], String)
              | Typ_attr [String]


data SYM_TABLE = Symbol_table (Int,Int,[(String,SYM_VALUE)])

type ST = [SYM_TABLE]