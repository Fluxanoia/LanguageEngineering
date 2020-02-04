module Statements where

    import AExp
    import BExp
    import Variables

    data Stm = Ass Var Aexp
        | Skip
        | Comp Stm Stm
        | If Bexp Stm Stm
        | While Bexp Stm
        deriving (Show, Eq, Read)
