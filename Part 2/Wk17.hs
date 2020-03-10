module Wk17 where

    data Primitive = Plus
                    | Minus
                    | Prod
                    deriving(Show)

    data Expr = EInt Int
                | Var  String
                | Let  String Expr Expr
                | Prim Primitive Expr Expr
                deriving(Show)

    data Expr_Ex = EIntx Int
                    | Varx String
                    | Letx [(String, Expr_Ex)] Expr_Ex
                    | Primx Primitive Expr_Ex Expr_Ex
                    deriving(Show)

    data TExpr = TInt Int 
           | TVar Int 
           | TLet TExpr TExpr
           | TPrim Primitive TExpr TExpr
           deriving(Show)

    data TExpr_Ex = TIntx Int 
           | TVarx Int 
           | TLetx TExpr_Ex TExpr_Ex
           | TPrimx Primitive TExpr_Ex TExpr_Ex
           deriving(Show)

    data StackValue = Value       
                    | Bound String
                    deriving(Eq)

    data SInstr = SInt Int
                | SVar Int
                | SAdd
                | SSub
                | SMul
                | SPop
                | SSwap
                deriving(Show)

    index_of :: Eq a => [a] -> a -> Int
    index_of [] x     = error "Variable not found."
    index_of (y:ys) x = if (x == y) then 0 else (1 + (index_of ys x))

-- 1

    scomp :: Expr -> [StackValue] -> [SInstr]
    scomp (EInt i) s = [SInt i]
    scomp (Var x) s  = [SVar (index_of s (Bound x))]
    scomp (Let x erhs ebody) s
        = (scomp erhs s) ++ (scomp ebody ((Bound x):s)) ++ [SSwap, SPop]
    scomp (Prim op e1 e2) s
        = case op of 
            Plus  -> scomp e1 s ++ scomp e2 (Value:s) ++ [SAdd] 
            Minus -> scomp e1 s ++ scomp e2 (Value:s) ++ [SSub] 
            Prod  -> scomp e1 s ++ scomp e2 (Value:s) ++ [SMul] 
            _     -> error "Unknown operator."

    seval :: [SInstr] -> [Int] -> Int 
    seval [] (v:vs)              = v 
    seval [] []                  = error "Empty stack."
    seval ((SInt i):insr) s      = seval insr (i:s)
    seval ((SVar i):insr) s      = seval insr ((s !! i):s)
    seval (SAdd:insr)  (i2:i1:s) = seval insr ((i1+i2):s)
    seval (SSub:insr)  (i2:i1:s) = seval insr ((i1-i2):s)
    seval (SMul:insr)  (i2:i1:s) = seval insr ((i1*i2):s)
    seval (SPop:insr)  (_:s)     = seval insr s
    seval (SSwap:insr) (i2:i1:s) = seval insr (i1:i2:s)
    seval _ _                       = error "Too few operands on stack."

-- 2

    tcomp :: Expr_Ex -> [String] -> TExpr_Ex
    tcomp (EIntx i) env = TIntx i
    tcomp (Varx x) env  = TVarx (index_of env x)
    tcomp (Letx ((x, ex):bs) ebody) env = let env1 = (x:env)
        in TLetx (tcomp ex env) (tcomp (Letx bs ebody) env1)
    tcomp (Letx [] ebody) env  = tcomp ebody env
    tcomp (Primx op e1 e2) env = TPrimx op (tcomp e1 env) (tcomp e2 env)

-- 3

-- SCST = 0
-- SVAR = 1
-- SADD = 2
-- SSUB = 3
-- SMUL = 4
-- SPOP = 5
-- SSWAP = 6

    translate :: SInstr -> Int
    translate SInt _ = 0
    translate SVar _ = 1
    translate SAdd   = 2
    translate SSub   = 3
    translate SMul   = 4
    translate SPop   = 5
    translate SSwap  = 6

    assemble :: [SInstr] -> [Int]
    assemble = map translate
