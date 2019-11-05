module State where

    import Numeral
    import Variables
        
    type State = Var -> Z
    
    -- Substitution Function
    
    subst_state :: State -> Z -> Var -> State
    subst_state s i v
        | (s v) == i = s
        | otherwise  = s' where
            s' :: State
            s' v'
                | v == v'   = i
                | otherwise = s v'
        
    subst_set_state :: State -> [Z] -> [Var] -> State
    subst_set_state s [] _          = s
    subst_set_state s _ []          = s
    subst_set_state s (z:zs) (v:vs) = subst_set_state 
                                        (subst_state s z v)
                                        zs vs
        