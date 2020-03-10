
{- Representing object language expressions using recursive datatypes -}

module Intro1 where

data Expr = CstI Int 
          | Prim String Expr Expr  
          deriving Show
          
e1 = CstI 17
e2 = Prim "-" (CstI 3) (CstI 4)
e3 = Prim "+" (Prim "*" (CstI 7) (CstI 9)) (CstI 10)


{- Evaluating expressions using recursive functions -}

eval :: Expr -> Int 
eval (CstI i)   = i
eval (Prim op e1 e2) 
    | op == "+" = eval e1 + eval e2 
    | op == "*" = eval e1 * eval e2
    | op == "-" = eval e1 - eval e2
    | otherwise = error "unknown primitive"

e1v = eval e1 
e2v = eval e2 
e3v = eval e3


{- Changing the meaning of subtraction -}

evalm :: Expr -> Int 
evalm (CstI i)  = i 
evalm (Prim op e1 e2)
    | op == "+" = eval e1 + eval e2 
    | op == "*" = eval e1 * eval e2
    | op == "-" = let res = evalm e1 - evalm e2
                  in  if res < 0 then 0 else res
    | otherwise = error "unknown primitive"

e4v = evalm (Prim "-" (CstI 10) (CstI 27))