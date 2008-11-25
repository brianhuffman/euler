module Euler076 where
import EulerLib
import Array

------------------------------------------------------------------------------
-- 76. How many different ways can one hundred be written as a sum of at least two positive integers?
{-
It is possible to write five as a sum in exactly six different ways:

4 + 1
3 + 2
3 + 1 + 1
2 + 2 + 1
2 + 1 + 1 + 1
1 + 1 + 1 + 1 + 1

How many different ways can one hundred be written as a sum of at least two positive integers?
-}

-- Partition numbers: See Problem 78.
-- solution = partition(100) - 1

-- p(n, m) = # of sums totalling n, with all terms <= m
-- p(0, m) = 1
-- p(n, m) = (SUM k=1..m. p(n-k,k))

sum_array :: Int -> Array (Int, Int) Int
sum_array k = a
  where
    a = funArray ((0,1),(k,k)) f
    f (0, m) = 1
    f (n, m)
      | m <= n = sum [ a!(n-i, i) | i <- [1 .. m] ]
      | otherwise = a!(n, n)

prob76 :: Int -> Int
prob76 n = sum_array n ! (n, n) - 1

main :: IO String
main = return $ show $ prob76 100
-- 190569291
