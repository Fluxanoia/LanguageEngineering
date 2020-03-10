{- File TypedFun/TypedFun.hs
   An explicitly typed strict first-order functional language. 

   Different abstract syntax from the first-order and higher-order 
   functional language in Fun/Fun.fs and Fun/HigherFun.hs because
   of the explicit types on function parameters and function results.

   Does not admit mutually recursive function bindings.

   Every function takes exactly one argument.

   Type checking.  Explicit types on the argument and result of each
   declared function.  Expressions and variables may have type int or
   bool or a functional type.  Functions are monomorphically and
   explicitly typed.  

   There is no lexer or parser specification for this explicitly typed
   language because next week we shall infer types rather than check
   them.  
-}

module TypedFun where

import Prelude hiding (lookup)

{- Environment operations -}

type Env a = [(String, a)]

lookup :: Env a -> String -> a 
lookup env x =
    case env of 
      []         -> error (x ++ " not found")
      ((y, v):r) -> if x == y then v else lookup r x

{- A type is int, bool or function -}


data Typ = TypI                                {- int                         -}
         | TypB                                {- bool                        -}
         | TypF Typ Typ                        {- argumentType resultType     -}
        deriving (Show, Eq)

{- New abstract syntax with explicit types, instead of Absyn.expr: -}

data TyExpr = CstI Int
            | CstB Bool
            | Var  String
            | Let  String TyExpr TyExpr
            | Prim String TyExpr TyExpr
            | If   TyExpr TyExpr TyExpr
            | Letfun String String Typ TyExpr Typ TyExpr {- f x xTyp fBody rTyp letBody -}
            | Call  TyExpr TyExpr
        deriving (Show, Eq)

{- A runtime value is an integer or a function closure -}

data Value = Num Int
           | Closure String String TyExpr (Env Value)       {- f x fBody fDeclEnv -}
        deriving (Show, Eq)

eval :: TyExpr -> Env Value -> Int
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
eval (Letfun f x _ fBody _ letBody) env =
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
eval (Call _ _) env = error "illegal function in Call"

{- Type checking for the first-order functional language: -}

typ :: TyExpr -> Env Typ -> Typ 
typ (CstI i) env = TypI 
typ (CstB b) env = TypB 
typ (Var  x) env = lookup env x
typ (Prim ope e1 e2) env = 
    let t1 = typ e1 env
        t2 = typ e2 env
    in case (ope, t1, t2) of
        ("*", TypI, TypI) -> TypI
        ("+", TypI, TypI) -> TypI
        ("-", TypI, TypI) -> TypI
        ("=", TypI, TypI) -> TypB
        ("<", TypI, TypI) -> TypB
        ("&", TypB, TypB) -> TypB
        _   -> error "unknown op, or type error"
typ (Let x eRhs letBody) env = 
    let xTyp        = typ eRhs env
        letBodyEnv  = ((x, xTyp):env)
    in  typ letBody letBodyEnv
typ (If e1 e2 e3) env = 
    case typ e1 env of 
        TypB -> let t2 = typ e2 env 
                    t3 = typ e3 env 
                in  if   t2 == t3 
                    then t2 
                    else error "If: branch types differ"
        _    -> error "If: condition not boolean"
typ (Letfun f x xTyp fBody rTyp letBody) env =
    let fTyp       = TypF xTyp rTyp
        fBodyEnv   = ((x, xTyp):(f, fTyp):env)
        letBodyEnv = ((f, fTyp):env)
    in  if   typ fBody fBodyEnv == rTyp 
        then typ letBody letBodyEnv 
        else error ("Letfun: return type in" ++ f)
typ (Call (Var f) eArg) env = 
    case lookup env f of 
        TypF xTyp rTyp -> if   typ eArg env == xTyp 
                          then rTyp 
                          else error "Call: wrong argument type"
        _              -> error "Call: unknown function"


typeCheck e = typ e []


{- Examples of successful type checking -}

ex1 = Letfun "f1" "x" TypI (Prim "+" (Var "x") (CstI 1)) TypI
             (Call (Var "f1") (CstI 12))

{- Factorial -}

ex2 = Letfun "fac" "x" TypI
                 (If (Prim "=" (Var "x") (CstI 0))
                     (CstI 1)
                     (Prim "*" (Var "x")
                               (Call (Var "fac") 
                                     (Prim "-" (Var "x") (CstI 1)) )))
                 TypI
                 (Let "n" (CstI 7) (Call (Var "fac") (Var "n")))

fac10 = eval ex2 []

ex3 = Let "b" (Prim "=" (CstI 1) (CstI 2))
              (If (Var "b") (CstI 3) (CstI 4))

ex4 = Let "b" (Prim "=" (CstI 1) (CstI 2))
              (If (Var "b") (Var "b") (CstB False))

ex5 = If (Prim "=" (CstI 11) (CstI 12)) (CstI 111) (CstI 666)

ex6 = Letfun "inf" "x" TypI (Call (Var "inf") (Var "x")) TypI
                 (Call (Var "inf") (CstI 0))

types = map typeCheck [ex1, ex2, ex3, ex4, ex5, ex6]

{- Examples of type errors; should throw exception when run: -}

exErr1 = Let "b" (Prim "=" (CstI 1) (CstI 2))
                 (If (Var "b") (Var "b") (CstI 6))

exErr2 = Letfun "f" "x" TypB (If (Var "x") (CstI 11) (CstI 22)) TypI
                    (Call (Var "f") (CstI 0))

exErr3 = Letfun "f" "x" TypB (Call (Var "f") (CstI 22)) TypI
                    (Call (Var "f") (CstB True))

exErr4 = Letfun "f" "x" TypB (If (Var "x") (CstI 11) (CstI 22)) TypB
                    (Call (Var "f") (CstB True))
