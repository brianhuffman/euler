module Euler179 where
import PrimeArray (num_divisors_array)
import Data.Array.Unboxed

{-
Problem 179
Consecutive positive divisors

26 January 2008

Find the number of integers 1 < n < 10^7, for which n and n + 1 have the same
number of positive divisors. For example, 14 has the positive divisors 1, 2,
7, 14 while 15 has 1, 3, 5, 15.
-}

prob179 m = length [ n | (n, n') <- zip ns (tail ns), n == n' ]
  where
    a = num_divisors_array m
    ns = [ a ! n | n <- [2 .. m] ]

-- 10^3: 118
-- 10^4: 1119
-- 10^5: 10585
-- 10^6: 102093

main :: IO String
main = return $ show $ prob179 (10^7)
-- 986262
