-- 19.1

-- R = ( (b)+ (a u e) )*

--     / a   / b   / Accepts?
-- q_0 / q_1 / q_0 / Yes
-- q_1 / F   / q_0 / Yes
-- F   / F   / F   / No

-- 19.2 / 19.3

-- let z = 17 in z + 2 * 3 end

-- Expr
-- let var '=' Expr in Expr end
-- let var '=' Expr in Expr '*' Expr end
-- let var '=' Expr in Expr '*' num end
-- let var '=' Expr in Expr '+' Expr '*' num end
-- let var '=' Expr in Expr '+' num '*' num end
-- let var '=' Expr in var '+' num '*' num end
-- let var '=' num in var '+' num '*' num end

-- let z = 17 in z + 2 * 3 end

-- 19.5

-- ghci Wk19.hs Parse.hs Absyn.hs ExprLex.hs ExprPar.hs

module Wk19 where

import Expr
import Parse

compString :: String -> [SInstr]
compString s = scomp (parseFromString s) []

-- 19.8

