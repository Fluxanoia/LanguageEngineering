module Parse where

import Absyn
import FunPar 
import FunLex

-- | Call generated expr lexer and parser on input string
-- | > ghci Parse.hs Absyn.hs FunLex.hs FunPar.hs

parseFromString :: String -> Expr
parseFromString s = (funParser . funLexer) s

parseFromFile :: String -> IO ()
parseFromFile filename = do 
    s <- readFile filename
    print $ parseFromString s 

-- Exercise it 

e1 = parseFromString "5+7"
e2 = parseFromString "let f x = x + 7 in f 2 end"

-- Examples in concrete syntax 

ex1 = parseFromString "let f1 x = x + 1 in f1 12 end"

-- Example: factorial 

ex2 = parseFromString "let fac x = if x=0 then 1 else x * fac(x - 1) \n\
                  \ in fac n end"

-- Example: deep recursion to check for constant-space tail recursion 

ex3 = parseFromString 
            "let deep x = if x=0 then 1 else deep(x-1) \n\
             \ in deep count end"
    
-- Example: static scope (result 14) or dynamic scope (result 25) 

ex4 = parseFromString 
             "let y = 11 \n\
              \ in let f x = x + y \n\
              \   in let y = 22 in f 3 end \n\ 
              \   end \n\
              \ end"

-- Example: two function definitions: a comparison and Fibonacci 

ex5 = parseFromString
             "let ge2 x = 1 < x \n\
              \ in let fib n = if ge2(n) then fib(n-1) + fib(n-2) else 1 \n\
              \    in fib 25  \n\
              \    end \n\
              \ end"
