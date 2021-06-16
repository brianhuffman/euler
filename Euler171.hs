module Euler171 where
import EulerLib
import Data.Array

{-
Problem 171
08 December 2007

For a positive integer n, let f(n) be the sum of the squares of the digits
(in base 10) of n, e.g.

f(3) = 3^2 = 9,
f(25) = 2^2 + 5^2 = 4 + 25 = 29,
f(442) = 4^2 + 4^2 + 2^2 = 16 + 16 + 4 = 36

Find the last nine digits of the sum of all n, 0 < n < 10^20, such that f(n)
is a perfect square.
-}

{-
For 20 digit numbers, sum of squares ranges from 0 to 1620.
Perfect squares in that range are 0^2 .. 40^2
-}

-- prob171a!(n,x) = how many n-digit strings have sum of squares equal to x?
prob171a :: Array (Int, Integer) Integer
prob171a = a
  where
    a = funArray ((0,0),(20,1620)) f
    f (0,x) = if x == 0 then 1 else 0
    f (n,x) = sum [ a!(n-1, x-d^2) | d <- [0 .. 9], d^2 <= x ]

-- (digit d, how many 20-digit strings summing to a square start with d)
prob171b =
  [ (d, sum [ prob171a!(19, n^2 - d^2) | n <- [d .. 40] ]) | d <- [0 .. 9] ]

main :: IO String
main = return $ show $
  (sum [ d * r | (d,r) <- prob171b ] * 11111111111111111111)
  `mod` (10^9)
-- 142989277
-- 104861799630145516743395826448142989277

-- TODO: generalize this code
