module Operational where

    import AExp
    import BExp
    import State
    import Variables
    import Statements
    import AbstractMachine
    
    data OS_Config = OS_Inter Stm State
        | OS_Final State
    
    -- Configuration Function
    
    os_stm :: OS_Config -> OS_Config
    os_stm (OS_Inter (Ass v a) s)        = OS_Final (subst_state s (a_val a s) v)
    os_stm (OS_Inter (Comp stm1 stm2) s) = os_stm (OS_Inter stm2 s') where
        OS_Final s' = os_stm (OS_Inter stm1 s)
    os_stm (OS_Inter (If b stm1 stm2) s)
        | (b_val b s) = os_stm (OS_Inter stm1 s)
        | otherwise   = os_stm (OS_Inter stm2 s)
    os_stm (OS_Inter (While b stm) s)
        | (b_val b s) = os_stm (OS_Inter (Comp (stm) (While b stm)) s)
        | otherwise   = OS_Final s
    os_stm (OS_Inter _ s) = OS_Final s
    os_stm (OS_Final s)   = OS_Final s
    
    -- Semantic Function
    
    s_os :: Stm -> State -> State
    s_os stm s = s' where
        OS_Final s' = os_stm (OS_Inter stm s)

    -- Translation Function

    trans_os_stm :: Stm -> Code
    trans_os_stm Skip         = AM_Noop:[]
    trans_os_stm (Ass v a)    = (trans_aexp a) ++ (AM_Store v):[]
    trans_os_stm (Comp s1 s2) = (trans_os_stm s1) ++ (trans_os_stm s2)
    trans_os_stm (If b s1 s2) = (trans_bexp b) ++ (AM_Branch (trans_os_stm s1) (trans_os_stm s2)):[]
    trans_os_stm (While b s)  = (AM_Loop (trans_bexp b) (trans_os_stm s)):[]

    -- Semantic Function under the Abstract Machine

    run_os :: Stm -> State -> State
    run_os stm s = am_execute (trans_os_stm stm) s
