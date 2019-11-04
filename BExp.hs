module BExp where

import AExp
import Truth
import State
import Variables

data Bexp = TRUE
    | FALSE
    | Neg Bexp
    | And Bexp Bexp
    | Eq Aexp Aexp
    | Leq Aexp Aexp
    deriving (Show, Eq, Read)

-- Semantic Function

b_val :: Bexp -> State -> Truth
b_val TRUE s        = True
b_val FALSE s       = False
b_val (Neg b) s     = not (b_val b s)
b_val (And b1 b2) s = (b_val b1 s) && (b_val b2 s)
b_val (Eq a1 a2) s  = (a_val a1 s) == (a_val a2 s)
b_val (Leq a1 a2) s = (a_val a1 s) <= (a_val a2 s)

-- Free Variable Function 

fv_bexp :: Bexp -> [Var]
fv_bexp (Neg b)     = fv_bexp b
fv_bexp (And b1 b2) = (fv_bexp b1) ++ (fv_bexp b2)
fv_bexp (Eq  a1 a2) = (fv_aexp a1) ++ (fv_aexp a2)
fv_bexp (Leq a1 a2) = (fv_aexp a1) ++ (fv_aexp a2)
fv_bexp _           = []

-- Substitution Function

subst_bexp :: Bexp -> Var -> Aexp -> Bexp
subst_bexp TRUE v r        = TRUE
subst_bexp FALSE v r       = FALSE
subst_bexp (Neg b) v r     = Neg (subst_bexp b v r)
subst_bexp (And b1 b2) v r = And (subst_bexp b1 v r) (subst_bexp b2 v r)
subst_bexp (Eq a1 a2) v r  = Eq (subst_aexp a1 v r) (subst_aexp a2 v r)
subst_bexp (Leq a1 a2) v r = Leq (subst_aexp a1 v r) (subst_aexp a2 v r)
