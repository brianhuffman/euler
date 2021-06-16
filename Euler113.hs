module Euler113 where
import EulerLib
import Data.Array

------------------------------------------------------------------------------
-- 113. How many numbers below a googol (10^100) are not "bouncy"?
{-
Working from left-to-right if no digit is exceeded by the digit to its left
it is called an increasing number; for example, 134468.

Similarly if no digit is exceeded by the digit to its right it is called a
decreasing number; for example, 66420.

We shall call a positive integer that is neither increasing nor decreasing a
"bouncy" number; for example, 155349.

As n increases, the proportion of bouncy numbers below n increases such that
there are only 12951 numbers below one-million that are not bouncy and only
277032 non-bouncy numbers below 10^10.

How many numbers below a googol (10^100) are not bouncy?
-}

-- # of monotone decreasing seqs, starting with d, length n
num_decreasing :: Int -> Array (Int, Int) Integer
num_decreasing m = a
  where
    a = funArray ((0,1),(9,m)) f
    f (d,1) = 1
    f (d,n) = sum [ a!(d',n-1) | d' <- [0..d] ]

-- # of non-bouncy numbers < 10^m
non_bouncy :: Int -> Integer
non_bouncy m = decreasing + increasing - constant
  where
    a = num_decreasing m
    constant = 9 * toInteger m
    decreasing = sum [ a!(d,n) | d <- [1..9], n <- [1..m] ]
    increasing = sum [ a!(d,n) | d <- [0..8], n <- [1..m] ]

main :: IO String
main = return $ show $ non_bouncy 100
-- 51161058134250

