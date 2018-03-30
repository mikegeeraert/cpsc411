module SymbolTable (ST,empty,new_scope,insert,lookup_decl,return_type, get_num_local_vars) where

import AST
import SymbolTypes
import Data.Monoid

data SYM_VALUE = Var_attr (Int,M_type,Int)
              | Fun_attr (String,[(M_type,Int)],M_type)
              | Con_attr (Int, [M_type], String)
              | Typ_attr [String]
              deriving (Eq, Show)


data SYM_TABLE = Symbol_table (ScopeType, Int,Int,[(String,SYM_VALUE)]) deriving (Show)

type ST = [SYM_TABLE]

empty = []

new_scope:: ScopeType -> ST -> ST 
new_scope scopeType st = (Symbol_table(scopeType, 0,0,[])):st

return_type :: ST -> M_type
return_type ((Symbol_table((L_FUN return_type), _, _, _)):_) = return_type
return_type st  = error ("Symbol table error: current scope not in function, therefore no return type can be found")

lookup_decl:: ST -> String -> SYM_I_DESC 
lookup_decl s str = find 0 s where

  find n [] = error ("Could not find "++ str)
  find n (Symbol_table(_,_,_,vs):rest) = 
         (case find_level str vs of 
            Just v -> found n v
            Nothing -> find (n+1) rest)  

  found level (Var_attr(offset, var_type, dim)) = I_VARIABLE(level, offset, var_type, dim)
  found level (Fun_attr(label, arg_types, return_type)) = I_FUNCTION(level, label, arg_types, return_type)

  find_level str ((existing_str,v):rest)
          | str == existing_str = Just v
          | otherwise =  find_level str rest
  find_level str [] = Nothing

insert:: Int -> ST -> SYM_DESC -> (Int,ST) 
insert n [] d =  error "Symbol table error: insertion before defining scope."
insert n ((Symbol_table(scopeType, nL, nA, sL)):rest) (ARGUMENT (str, t, dim)) 
         | (in_index_list str sL) = error ("Symbol table error: " ++ str ++"is already defined.")
         | otherwise              = (n, Symbol_table(scopeType, nL, nA+1 , (str, Var_attr((nA+4), t, dim)):sL):rest )
insert n ((Symbol_table(scopeType, nL, nA, sL)):rest) (FUNCTION (str, ts, t))
         | in_index_list str sL = error ("Symbol table error: "++str++"is already defined.")
         | otherwise            = (n+1,(Symbol_table(scopeType, nL, nA, (str, Fun_attr(getLabel n str, ts, t)):sL)):rest )
insert n ((Symbol_table(scopeType, nL, nA, sL)):rest) (VARIABLE (str, t, dim)) 
         | (in_index_list str sL) = error ("Symbol table error: "++ str ++"is already defined.")
         | otherwise              = (n,Symbol_table(scopeType, nL+1, nA ,(str, Var_attr(nL+1, t, dim)):sL):rest ) 

in_index_list:: String -> [(String, SYM_VALUE)] -> Bool
in_index_list str [] = False
in_index_list str ((x,_):xs)
  | str==x = True
  | otherwise = in_index_list str xs

getLabel:: Int -> String -> String
getLabel n str = str ++ "_" ++ show(n)

get_num_local_vars:: ST -> Int
get_num_local_vars (Symbol_table(_, numLocalVars, _, _):_) = numLocalVars

--TODO
get_array_specifications :: ST -> ()
get_array_specifications = undefined
