module Euler115 where

------------------------------------------------------------------------------
-- 115. Finding a generalisation for the number of ways to fill a row with separated blocks.
{-
NOTE: This is a more difficult version of problem 114.

A row measuring n units in length has red blocks with a minimum length of m
units placed on it, such that any two red blocks (which are allowed to be
different lengths) are separated by at least one black square.

Let the fill-count function, F(m, n), represent the number of ways that a row
can be filled.

For example, F(3, 29) = 673135 and F(3, 30) = 1089155.

That is, for m = 3, it can be seen that n = 30 is the smallest value for which
the fill-count function first exceeds one million.

In the same way, for m = 10, it can be verified that F(10, 56) = 880711 and
F(10, 57) = 1148904, so n = 57 is the least value for which the fill-count
function first exceeds one million.

For m = 50, find the least value of n for which the fill-count function first
exceeds one million.
-}

{-
For total length n+1,
* Start with black, followed by length (n)
* One red covering the whole thing (if n+1 >= m)
* Start with red length k>=m, then one black, then length (n-k)

Recurrence relation:
F(m, 0) = 1
F(m, n+1) =
  F(m, n) +
  (if n+1 >= m then 1 else 0) +
  SUM k:[0 .. n-m]. F(m, k)
-}

fill_count :: Int -> [Integer]
fill_count m = xs
  where xs = 1 : zipWith (+) xs (replicate (m-1) 0 ++ scanl (+) 1 xs)
-- fill_count 3 !! 29 = 673135
-- fill_count 3 !! 30 = 1089155

prob115 :: Int -> Integer -> Int
prob115 m n = length $ takeWhile (< n) $ fill_count m
-- prob115 3 = 30
-- prob115 10 = 57
-- prob115 50 = 168

main :: IO String
main = return $ show $ prob115 50 (10^6)
-- 168
