{- # Abstract syntax for micro-ML, a functional language # -}

module Absyn where

data Expr = CstI Int 
          | CstB Bool
          | Var String 
          | Let String Expr Expr 
          | Prim String Expr Expr 
          | If Expr Expr Expr 
          | Letfun String String Expr Expr  -- f x fBody letBody
          | Call Expr Expr
          deriving Show