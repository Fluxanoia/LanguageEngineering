module AExp where

    import State
    import Numeral
    import Variables
    import AbstractMachine
    
    data Aexp = N Numeral
        | V Var
        | Add Aexp Aexp
        | Sub Aexp Aexp
        | Mult Aexp Aexp
        deriving (Show, Eq, Read)
    
    -- Semantic Function
    
    a_val :: Aexp -> State -> Z
    a_val (N i) s        = n_val i
    a_val (V v) s        = s v
    a_val (Add a1 a2) s  = (a_val a1 s) + (a_val a2 s) 
    a_val (Sub a1 a2) s  = (a_val a1 s) - (a_val a2 s) 
    a_val (Mult a1 a2) s = (a_val a1 s) * (a_val a2 s) 
    
    -- Free Variable Function
    
    fv_aexp :: Aexp -> [Var]
    fv_aexp (N _)        = []
    fv_aexp (V v)        = [v]
    fv_aexp (Add a1 a2)  = (fv_aexp a1) ++ (fv_aexp a2)
    fv_aexp (Sub a1 a2)  = (fv_aexp a1) ++ (fv_aexp a2)
    fv_aexp (Mult a1 a2) = (fv_aexp a1) ++ (fv_aexp a2)
    
    -- Substitution Function
    
    subst_aexp :: Aexp -> Var -> Aexp -> Aexp
    subst_aexp (V var) v r
        | var == v  = r
        | otherwise = (V var)
    subst_aexp (N i) v r        = (N i)
    subst_aexp (Add a1 a2) v r  = Add (subst_aexp a1 v r) (subst_aexp a2 v r)
    subst_aexp (Sub a1 a2) v r  = Sub (subst_aexp a1 v r) (subst_aexp a2 v r)
    subst_aexp (Mult a1 a2) v r = Mult (subst_aexp a1 v r) (subst_aexp a2 v r)
    
    -- Translation Function

    trans_aexp :: Aexp -> Code
    trans_aexp (N i)        = [AM_Push i]
    trans_aexp (V v)        = [AM_Fetch v]
    trans_aexp (Add a1 a2)  = (trans_aexp a2) ++ (trans_aexp a1) ++ AM_Add:[]
    trans_aexp (Sub a1 a2)  = (trans_aexp a2) ++ (trans_aexp a1) ++ AM_Sub:[]
    trans_aexp (Mult a1 a2) = (trans_aexp a2) ++ (trans_aexp a1) ++ AM_Mult:[]
