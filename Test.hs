module Test where

    import AExp
    import BExp
    import State
    import Operational
    
    -- State representing:
    --     x -> 1
    --     y -> 2
    --     z -> 3
    --     0 otherwise
    s :: State
    s "x" = 1
    s "y" = 2
    s "z" = 3
    s _   = undefined
    
    s' :: State
    s' = subst_set_state s [17, 5] ["x", "y"]
    
    -- Aexp representing (x + y) - (z - 1)
    a :: Aexp
    a = Mult (Add (V "x") (V "y")) (Sub (V "z") (N 1))
    
    -- Bexp representing !((x + y) == 4)
    b :: Bexp
    b = Neg (Eq (Add (V "x") (V "y")) (N 4))
    
    -- Stm representing
    -- z = 0
    -- while (y <= x) {
    --   z = z + 1
    --   x = x - y
    -- }
    --
    -- z is the number of times y goes into x
    -- x is the remainder
    p :: OS_Stm
    p = OS_Comp (OS_Ass "z" (N 0)) 
            (OS_While (Leq (V "y") (V "x")) 
                (OS_Comp 
                    (OS_Ass "z" 
                        (Add (V "z") (N 1))
                    )
                    (OS_Ass "x"
                        (Sub (V "x") (V "y"))
                    )
                )
            )