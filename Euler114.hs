module Euler114 where

------------------------------------------------------------------------------
-- 114. Investigating the number of ways to fill a row with separated blocks that are at least three units long.
{-
A row measuring seven units in length has red blocks with a minimum length of
three units placed on it, such that any two red blocks (which are allowed to
be different lengths) are separated by at least one black square. There are
exactly seventeen ways of doing this.

xxxxxxx  ---xxxx  x---xxx
xx---xx  xxx---x  xxxx---
---x---  ----xxx  x----xx
xx----x  xxx----  -----xx
x-----x  xx-----  ------x
x------  -------

How many ways can a row measuring fifty units in length be filled?

NOTE: Although the example above does not lend itself to the possibility, in
general it is permitted to mix block sizes. For example, on a row measuring
eight units in length you could use red (3), black (1), and red (4).
-}

{-
valid lengths are 1, 4, 5, 6...
f(0) = 1
f(1) = 1 = f(0)
f(2) = 1 = f(1)
f(3) = 2 = f(2)+1
f(4) = 4 = f(3)+f(0)+1
f(5) = 7 = f(4)+f(1)+f(0)+1
f(6) = 11 = f(5)+f(2)+f(1)+f(0)+1
f(7) = 17 = f(6)+f(3)+f(2)+f(1)+f(0)+1
-}

{-
a:  1, 1, 1, 2, 4, 7,11,17,27
b:  1, 1, 2, 4, 7,11,17,27,44
c:  1, 2, 4, 7,11,17,27,44,72
t:  1, 2, 3, 4, 6,10,17,28,45

a' = b
b' = c
c' = c+t
t' = t+a

a_n = F(n)
b_n = F(n+1)
c_n = F(n+2)
t_n = 1 + SUM i=0..n-1. F(i)
-}

fill3 :: [Integer]
fill3 = f 1 1 1 1
  where f a b c t = a : f b c (c+t) (t+a)

main :: IO String
main = return $ show $ fill3 !! 50
-- 16475640049
