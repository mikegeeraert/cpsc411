module SymbolTable (ST,empty,new_scope,insert,lookup_decl,return_type) where

import AST
import SymbolTypes
import IR
import Data.Monoid

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
getLabel n str = "code_label_" ++ str


data SYM_DESC = ARGUMENT (String, M_type, Int)
              | FUNCTION (String, [(M_type, Int)], M_type)
              | VARIABLE (String, M_type, Int)
              | DATA (String)
              | CONSTRUCTOR (String, [M_type], M_type)

data SYM_I_DESC = I_VARIABLE (Int,Int,M_type,Int)
                | I_FUNCTION (Int,String,[(M_type,Int)],M_type)
                | I_CONSTRUCTOR (Int,[M_type],String)
                | I_TYPE [String]