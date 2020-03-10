module Wk14 where

        ----------------
        -- Data Types --
        ----------------

    type Variable = (String, Int)
    type Binding = (String, Expr)
    type Env = [Variable]

    data Primitive = Plus
                   | Minus
                   | Prod
                   | Max
                   | Min
                   | Eq
                   deriving(Show, Eq)

    data Expr = Integer Int
              | Var String
              | Prim Primitive Expr Expr
              | If Expr Expr Expr
              | Let [Binding] Expr
              deriving(Eq)

        -----------------
        -- Evalutation --
        -----------------

    eval :: Expr -> Env -> Int
    eval (Integer i) e = i
    eval (Var v) e     = inspect e v
    eval (Prim op e1 e2) e
        | op == Plus  = (eval e1 e) + (eval e2 e)
        | op == Prod  = (eval e1 e) * (eval e2 e)
        | op == Minus = (eval e1 e) - (eval e2 e)
        | op == Max   = max (eval e1 e) (eval e2 e)
        | op == Min   = min (eval e1 e) (eval e2 e)
        | op == Eq    = if (eval e1 e) == (eval e2 e) then 1 else 0
        | otherwise   = error "Unknown Operator for Evaluation"
    eval (If e1 e2 e3) e
        | (eval e1 e) /= 0 = eval e2 e
        | otherwise        = eval e3 e
    -- Simultaeneous Letting
    eval (Let bs ex) env = let (env1, _) = foldl f (env, env) bs
                                in eval ex env1
                            --    f :: (Env, Env) -> Binding -> (Env, Env)
                            where f (env', env) (x, xexpr) = let xval = eval xexpr env
                                                                in (((x, xval):env'), env)
    -- Sequential Letting
    -- eval (Let ((s, bind):bs) ex) env = let env' = ((s, eval bind env):env)
    --                                     in eval (Let bs ex) env'
    -- eval (Let [] ex) env             = eval ex env 
    eval _ _ = error "Unknown Expression for Evaluation"

    weak_eval :: Expr -> Expr
    weak_eval (Prim Plus e1 e2) = case (weak_eval e1, weak_eval e2) of
        (Integer 0, v2)          -> v2
        (v1, Integer 0)          -> v1
        (Integer i1, Integer i2) -> if (i1 == (-i2)) then Integer 0 else (Prim Plus (Integer i1) (Integer i2))
        (v1, v2)                 -> Prim Plus v1 v2
    weak_eval (Prim Minus e1 e2) = case (weak_eval e1, weak_eval e2) of
        (Integer 0, v2)          -> Prim Prod (Integer (-1)) v2
        (v1, Integer 0)          -> v1
        (Integer i1, Integer i2) -> if i1 == i2 then Integer 0 else (Prim Minus (Integer i1) (Integer i2))
        (v1, v2)                 -> Prim Minus v1 v2
    weak_eval (Prim Prod e1 e2) = case (weak_eval e1, weak_eval e2) of
        (Integer 0, v2) -> Integer 0
        (v1, Integer 0) -> Integer 0
        (Integer 1, v2) -> v2
        (v1, Integer 1) -> v1
        (v1, v2)        -> Prim Prod v1 v2
    weak_eval (If e1 e2 e3) = case weak_eval e1 of
        Integer 0 -> weak_eval e3
        Integer _ -> weak_eval e2
        otherwise -> If e1 e2 e3
    -- Sequential / Simultaeneous Letting
    weak_eval e = e

        -----------
        -- I / O --
        -----------

    format_expr :: Expr -> String
    format_expr (Integer i)          = show i
    format_expr (Var v)              = "Var " ++ v
    format_expr (Prim op e1 e2)      = "(" ++ (format_expr e1) ++ " " ++ (show op) ++ " " ++ (format_expr e2) ++ ")"
    format_expr (If e1 e2 e3)        = "(If " ++ (format_expr e1) ++ " then " ++ (format_expr e2) ++ " else " ++ (format_expr e3) ++ ")"
    format_expr (Let ((s, b):bs) ex) = "(Let " ++ s ++ " = " ++ (format_expr b) ++ " in " ++ (format_expr (Let bs ex)) ++ ")"
    format_expr (Let [] ex)          = format_expr ex
    format_expr _                    = error "Unknown Expression for Formatting"

        --------------------
        -- Set Operations --
        --------------------

    member :: Eq a => a -> [a] -> Bool
    member x []     = False 
    member x (y:ys) = (x == y) || (member x ys)

    union :: Eq a => [a] -> [a] -> [a]
    union [] ys     = ys
    union (x:xs) ys
        | member x ys = union xs ys
        | otherwise   = union xs (x:ys)

    set_minus :: Eq a => [a] -> [a] -> [a]
    set_minus [] ys     = []
    set_minus (x:xs) ys
        | member x ys = set_minus xs ys 
        | otherwise   = x:(set_minus xs ys)

        -------------------------------------
        -- Environment / Binding Tampering --
        -------------------------------------

    inspect :: Env -> String -> Int
    inspect [] x = error ("Variable " ++ x ++ " does not exist in the Environment")
    inspect ((y, v) : r) x
        | x == y    = v
        | otherwise = inspect r x

    subst :: [Binding] -> String -> Expr
    subst [] x          = Var x
    subst ((y, e):bs) x = if (x == y) then e else (subst bs x)

    -- eval_subst :: Expr -> [Binding] -> Expr
    -- eval_subst (Integer i) e = return (Integer i)
    -- eval_subst (Var v) e     = return (subst e v)
    -- eval_subst (Prim op e1 e2) e = Prim op (eval_subst e1 e) (eval_subst e2 e)
    -- eval (If e1 e2 e3) e = If (eval_subst e1 e) (eval_subst e2 e) (eval_subst e3 e)
    -- -- Simultaeneous Letting
    -- -- TODO
    -- -- Sequential Letting
    -- -- TODO
    -- eval _ _ = error "Unknown Expression for Substitution"

    remove :: [Binding] -> String -> [Binding]
    remove [] x = []
    remove ((y, e):bs) x = if (x == y) then bs 
        else ((y, e):(remove bs x))

    remove_many :: [Binding] -> [String] -> [Binding]
    remove_many [] xs     = []
    remove_many bs (x:xs) = remove_many (remove bs x) xs

        --------------------
        -- Free Variables --
        --------------------

    freevars :: Expr -> [String]
    freevars (Integer i)             = []
    freevars (Var v)                 = [v]
    freevars (Prim _ e1 e2)          = (freevars e1) ++ (freevars e2)
    freevars (Let ((s, bind):bs) ex) = union (freevars bind) (set_minus (freevars (Let bs ex)) [s])
    freevars (Let [] ex)             = freevars ex 
    freevars _                       = error "Unknown Expression for Free Variables"

    closed :: Expr -> Bool
    closed e = (freevars e) == []
