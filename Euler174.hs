module Euler174 where
import PrimeArray (num_divisors_array)
import Primes
import Data.Array.Unboxed

{-
Problem 174
22 December 2007

We shall define a square lamina to be a square outline with a square "hole"
so that the shape possesses vertical and horizontal symmetry.

Given eight tiles it is possible to form a lamina in only one way: 3x3 square
with a 1x1 hole in the middle. However, using thirty-two tiles it is possible
to form two distinct laminae.

XXXXXX  XXXXXXXXX
XXXXXX  X       X
XX  XX  X       X
XX  XX  X       X
XXXXXX  X       X
XXXXXX  X       X
        X       X
        X       X
        XXXXXXXXX

If t represents the number of tiles used, we shall say that t = 8 is type L(1)
and t = 32 is type L(2).

Let N(n) be the number of t <= 1000000 such that t is type L(n); for example,
N(15) = 832.

What is SUM N(n) for 1 <= n <= 10?
-}

{-
See also Problem 173.

Number of tiles, t = 4d (a + d)
where a = width of inner square, a > 0
  and d = thickness of frame

t must be a multiple of 4.
L(t) = half of number of factors of n/4
(if t/4 is square, then the square root doesn't count)

L(t) of arrangements <= 10  <--> num_divisors(n/4) <= 21
-}

prob174 l m = length
  [ (4*k, c) |
    k <- [2 .. m `div` 4],
    let c = (a!k) `div` 2,
    c <= l ]
  where a = num_divisors_array (m `div` 4)

main :: IO String
main = return $ show $ prob174 10 (10^6)
-- 209566
