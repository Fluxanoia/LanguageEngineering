module Numeral where

    type Numeral = Integer
    type Z = Integer
    
    -- Semantic Function
    
    n_val :: Numeral -> Z
    n_val = id