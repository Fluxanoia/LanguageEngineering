import AbstractMachine
import AExp
import BExp
import Denotational
import Numeral
import Operational
import State
import Statements
import Test
import Truth
import Variables

-- Fibonacci Function

fib :: Int -> Int
fib = fix fib_fix where
    fib_fix :: (Int -> Int) -> (Int -> Int)
    fib_fix f n
        | (n == 0) || (n == 1) = 1
        | otherwise            = (f (n - 1)) + (f (n - 2))
