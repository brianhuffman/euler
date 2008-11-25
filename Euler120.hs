module Euler120 where

------------------------------------------------------------------------------
-- 120. Finding the maximum remainder when (a − 1)^n + (a + 1)^n is divided by a^2.
{-
Let r be the remainder when (a-1)^n +(a+1)^n is divided by a^2.

For example, if a = 7 and n = 3, then r = 42: 6^3 + 8^3 = 728 == 42 mod 49.
And as n varies, so too will r, but for a = 7 it turns out that rmax = 42.

For 3 <= a <= 1000, find SUM rmax.
-}

{-
See also Problem 123.

Use binomial expansion:
(a + b)^n == n*a*b^(n-1) + b^n  (mod a^2)

(a+1)^n == n*a + 1  (mod a^2)
(a-1)^n == 1 - n*a  (mod a^2)  (for even n)
(a-1)^n == n*a - 1  (mod a^2)  (for odd n)
(a+1)^n + (a-1)^n == 2      (mod a^2)  (for even n)
(a+1)^n + (a-1)^n == 2*n*a  (mod a^2)  (for odd n)

Property of mod: (a*c `mod` b*c) = (a `mod` b) * c

2*n*a `mod` a*a = (2*n `mod` a) * a

rmax = a * maximum [ 2*n `mod` a | odd n ]
rmax = a * (a - 2)  (for even a)
rmax = a * (a - 1)  (for odd a)
-}

rmax :: Integer -> Integer
rmax a
  | even a = a * (a - 2)
  | odd a  = a * (a - 1)

prob120 :: Integer -> Integer
prob120 n
  | even n = (4*n^3 - 3*n^2 - 10*n) `div` 12
  | odd n = sum [ rmax a | a <- [3 .. n] ]

{-
Closed form for sum [ rmax a | a <- [3 .. 2*k] ]

sum [ rmax a | a <- [3 .. 2*k] ] = (8*k^3 - 3*k^2 - 5*k) `div` 3

8/3*k^3 - k^2 - 5/3*k
8/3*(n/2)^3 - (n/2)^2 - 5/3*(n/2)
1/3*n^3 - 1/4*n^2 - 5/6*n
4/12*n^3 - 3/12*n^2 - 10/12*n
4/12*n^3 - 3/12*n^2 - 10/12*n
-}

main :: IO String
main = return $ show $ prob120 1000
-- 333082500

