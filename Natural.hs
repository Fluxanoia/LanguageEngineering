module Natural where

import AExp
import BExp
import State
import Variables

data Stm = Ass Var Aexp
    | Skip
    | Comp Stm Stm
    | If Bexp Stm Stm
    | While Bexp Stm
    deriving (Show, Eq, Read)

data OS_Config = Inter Stm State
    | Final State

-- Configuration Function

ns_stm :: OS_Config -> OS_Config
ns_stm (Inter (Ass v a) s)        = Final (subst_state s (a_val a s) v)
ns_stm (Inter (Comp stm1 stm2) s) = ns_stm (Inter stm2 s') where
    Final s' = ns_stm (Inter stm1 s)
ns_stm (Inter (If b stm1 stm2) s)
    | (b_val b s) = ns_stm (Inter stm1 s)
    | otherwise   = ns_stm (Inter stm2 s)
ns_stm (Inter (While b stm) s)
    | (b_val b s) = ns_stm (Inter (Comp (stm) (While b stm)) s)
    | otherwise   = Final s
ns_stm (Inter _ s) = Final s
ns_stm (Final s)   = Final s

-- Semantic Function

s_ns :: Stm -> State -> State
s_ns stm s = s' where
    Final s' = ns_stm (Inter stm s)