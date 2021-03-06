module AbstractMachine where

    import State
    import Truth
    import Numeral
    import Variables

    type Code = [Instruction]
    data Instruction = AM_Push Numeral
                       | AM_Add
                       | AM_Mult
                       | AM_Sub
                       | AM_True
                       | AM_False
                       | AM_Eq
                       | AM_Leq
                       | AM_And
                       | AM_Neg
                       | AM_Fetch Var
                       | AM_Store Var
                       | AM_Noop
                       | AM_Branch Code Code
                       | AM_Loop Code Code
                       deriving (Show, Eq, Read)
    type Stack = [Stack_Value]
    data Stack_Value = AM_A Z | AM_B Truth deriving (Show, Eq, Read)
    type AM_Config = (Code, Stack, State)

    -- Step Function

    am_step :: AM_Config -> AM_Config
    am_step ((AM_Push n):cs, xs, s)                 = (cs, (AM_A (n_val n)):xs, s)
    am_step (AM_Add:cs, (AM_A n1):(AM_A n2):xs, s)  = (cs, (AM_A (n1 + n2)):xs, s)
    am_step (AM_Mult:cs, (AM_A n1):(AM_A n2):xs, s) = (cs, (AM_A (n1 * n2)):xs, s)
    am_step (AM_Sub:cs, (AM_A n1):(AM_A n2):xs, s)  = (cs, (AM_A (n1 - n2)):xs, s)
    am_step (AM_True:cs, xs, s)                     = (cs, (AM_B True):xs, s)
    am_step (AM_False:cs, xs, s)                    = (cs, (AM_B False):xs, s)
    am_step (AM_Eq:cs, (AM_A n1):(AM_A n2):xs, s)   = (cs, (AM_B (n1 == n2)):xs, s)
    am_step (AM_Leq:cs, (AM_A n1):(AM_A n2):xs, s)  = (cs, (AM_B (n1 <= n2)):xs, s)
    am_step (AM_And:cs, (AM_B b1):(AM_B b2):xs, s)  = (cs, (AM_B (b1 && b2)):xs, s)
    am_step (AM_Neg:cs, (AM_B b):xs, s)             = (cs, (AM_B (not b)):xs, s)
    am_step ((AM_Fetch v):cs, xs, s)                = (cs, (AM_A (s v)):xs, s)
    am_step ((AM_Store v):cs, (AM_A a):xs, s)       = (cs, xs, subst_state s a v)
    am_step ((AM_Branch c1 c2):cs, (AM_B b):xs, s)
        | b         = (c1 ++ cs, xs, s)
        | otherwise = (c2 ++ cs, xs, s)
    am_step ((AM_Loop c1 c2):cs, xs, s)             = (c1 ++ [(AM_Branch 
                                                        (c2 ++ [(AM_Loop c1 c2)]) 
                                                        [(AM_Noop)]
                                                    )] ++ cs, xs, s)
    am_step (AM_Noop:cs, xs, s)                     = (cs, xs, s)
    am_step _                                       = undefined  

    am_info :: AM_Config -> Int -> IO ()
    am_info x n = print (c', e') where
        (c', e', s') = (foldr (.) id (replicate n am_step)) x

    am_comp_seq :: Code -> State -> AM_Config
    am_comp_seq [] s = ([], [], s)
    am_comp_seq cs s = last (am_comp_seq_recur (cs, [], s)) where
        am_comp_seq_recur :: AM_Config -> [AM_Config]
        am_comp_seq_recur ([], e, s)   = [([], e, s)]
        am_comp_seq_recur (c:cs, e, s) = (c:cs, e, s):(am_comp_seq_recur (am_step (c:cs, e, s)))

    am_execute :: Code -> State -> State
    am_execute cs s = s' where
        (_, _, s') = am_comp_seq cs s
