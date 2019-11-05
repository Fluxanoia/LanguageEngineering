module Operational where

    import AExp
    import BExp
    import State
    import Variables
    import AbstractMachine
    
    data OS_Stm = OS_Ass Var Aexp
        | OS_Skip
        | OS_Comp OS_Stm OS_Stm
        | OS_If Bexp OS_Stm OS_Stm
        | OS_While Bexp OS_Stm
        deriving (Show, Eq, Read)
    
    data OS_Config = OS_Inter OS_Stm State
        | OS_Final State
    
    -- Configuration Function
    
    os_stm :: OS_Config -> OS_Config
    os_stm (OS_Inter (OS_Ass v a) s)        = OS_Final (subst_state s (a_val a s) v)
    os_stm (OS_Inter (OS_Comp stm1 stm2) s) = os_stm (OS_Inter stm2 s') where
        OS_Final s' = os_stm (OS_Inter stm1 s)
    os_stm (OS_Inter (OS_If b stm1 stm2) s)
        | (b_val b s) = os_stm (OS_Inter stm1 s)
        | otherwise   = os_stm (OS_Inter stm2 s)
    os_stm (OS_Inter (OS_While b stm) s)
        | (b_val b s) = os_stm (OS_Inter (OS_Comp (stm) (OS_While b stm)) s)
        | otherwise   = OS_Final s
    os_stm (OS_Inter _ s) = OS_Final s
    os_stm (OS_Final s)   = OS_Final s
    
    -- Semantic Function
    
    s_os :: OS_Stm -> State -> State
    s_os stm s = s' where
        OS_Final s' = os_stm (OS_Inter stm s)

    -- Translation Function

    trans_os_stm :: OS_Stm -> Code
    trans_os_stm OS_Skip         = AM_Noop:[]
    trans_os_stm (OS_Ass v a)    = (trans_aexp a) ++ (AM_Store v):[]
    trans_os_stm (OS_Comp s1 s2) = (trans_os_stm s1) ++ (trans_os_stm s2)
    trans_os_stm (OS_If b s1 s2) = (trans_bexp b) ++ (AM_Branch (trans_os_stm s1) (trans_os_stm s2)):[]
    trans_os_stm (OS_While b s)  = (AM_Loop (trans_bexp b) (trans_os_stm s)):[]

    -- Semantic Function under the Abstract Machine

    run_os :: OS_Stm -> State -> State
    run_os stm s = am_execute (trans_os_stm stm) s
