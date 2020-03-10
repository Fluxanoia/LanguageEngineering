{- # Abstract syntax for the simple expression language # -}

module Absyn where 

import ExprLex

data Expr = CstI Int 
          | Var  String 
          | Let  String Expr Expr
          | Prim String Expr Expr
          deriving Show