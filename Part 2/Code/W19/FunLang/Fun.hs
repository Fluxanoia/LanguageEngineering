 
module Fun where

import Absyn
import Prelude hiding (lookup)

{- Environment operations -}

type Env a = [(String, a)]

lookup :: Env a -> String -> a 
lookup env x =
    case env of 
      []         -> error (x ++ " not found")
      ((y, v):r) -> if x == y then v else lookup r x

{- A runtime value is an integer or a function closure -}

data Value = Num Int
           | Closure String String Expr (Env Value)       -- f x fBody fDeclEnv 

eval :: Expr -> Env Value -> Int
eval (CstI i) env = i 
eval (CstB b) env = if b then 1 else 0 
eval (Var  x) env = 
    case lookup env x of 
      Num i -> i 
      _     -> error "eval Var"
eval (Prim ope e1 e2) env = 
    let i1 = eval e1 env
        i2 = eval e2 env
    in case ope of
        "*" -> i1 * i2
        "+" -> i1 + i2
        "-" -> i1 - i2
        "=" -> if i1 == i2 then 1 else 0
        "<" -> if i1 < i2 then 1 else 0
        _   -> error ("unknown primitive " ++ ope)
eval (Let x eRhs letBody) env = 
    let xVal    = Num (eval eRhs env)
        bodyEnv = ((x, xVal):env)
    in  eval letBody bodyEnv
eval (If e1 e2 e3) env = 
    let b = eval e1 env
    in  if b /= 0 
        then eval e2 env
        else eval e3 env
eval (Letfun f x fBody letBody) env =
    let bodyEnv = ((f, Closure f x fBody env) : env)
    in  eval letBody bodyEnv
eval (Call (Var f) eArg) env = 
    let fClosure = lookup env f
    in  case fClosure of
         Closure f x fBody fDeclEnv ->
            let xVal = Num (eval eArg env)
                fBodyEnv = ((x, xVal):(f, fClosure):fDeclEnv)
            in  eval fBody fBodyEnv
         _ -> error "eval Call: not a function"
eval (Call _ _) env = error "eval Call: not first-order function"

{- Evaluate in empty environment: program must have no free variables: -}

run e = eval e []

{- Examples in abstract syntax -}

ex1 = Letfun "f1" "x" (Prim "+" (Var "x") (CstI 1)) (Call (Var "f1") (CstI 12))

{- Example: factorial -}

ex2 = Letfun "fac" "x" (If (Prim "=" (Var "x") (CstI 0))
                           (CstI 1)
                           (Prim "*" (Var "x") (Call (Var "fac") (Prim "-" (Var "x") (CstI 1)))))
                        (Call (Var "fac") (Var "n"))

{- fac10 = eval ex2 [("n", Num 10)] -}

{- Example: deep recursion to check for constant-space tail recursion -}

ex3 = Letfun "deep" "x" 
                 (If (Prim "=" (Var "x") (CstI 0))
                     (CstI 1)
                     (Call (Var "deep") (Prim "-" (Var "x") (CstI 1))))
                 (Call (Var "deep") (Var "count"))
    
rundeep n = eval ex3 [("count", Num n)]

{- Example: static scope (result 14) or dynamic scope (result 25)  -}

ex4 =
    Let "y" (CstI 11)
        (Letfun "f" "x" (Prim "+" (Var "x") (Var "y"))
               (Let "y" (CstI 22) (Call (Var "f") (CstI 3))))

{- Example: two function definitions: a comparison and Fibonacci  -}

ex5 = 
    Letfun "ge2" "x" (Prim "<" (CstI 1) (Var "x"))
           (Letfun "fib" "n"
                   (If (Call (Var "ge2") (Var "n"))
                       (Prim "+"
                             (Call (Var "fib") (Prim "-" (Var "n") (CstI 1)))
                             (Call (Var "fib") (Prim "-" (Var "n") (CstI 2))))
                       (CstI 1)) 
                   (Call (Var "fib") (CstI 25)))
                     
