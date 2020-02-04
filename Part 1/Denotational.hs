module Denotational where

    import AExp
    import BExp
    import Truth
    import State
    import Statements

    -- Semantic Function

    s_ds :: Stm -> State -> State
    s_ds (Ass v a) s     = subst_state s (a_val a s) v
    s_ds Skip s          = s
    s_ds (Comp s1 s2) s  = s_ds s2 (s_ds s1 s)
    s_ds (If b s1 s2) s  = cond ((b_val b), (s_ds s1), (s_ds s2)) s
    s_ds (While b stm) s = fix gen s where
        gen :: (State -> State) -> (State -> State)
        gen g = cond (b_val b, g . (s_ds stm), id)

    -- Conditional Operator

    cond :: (a -> Truth, a -> a, a -> a) -> (a -> a)
    cond (p, f, g) a
        | (p a) == True = f a
        | otherwise     = g a

    -- Fixed Point Operator
        
    fix :: ((a -> a) -> (a -> a)) -> (a -> a)
    fix f = f (fix f)