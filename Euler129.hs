module Euler129 where
import Primes

------------------------------------------------------------------------------
-- 129. Investigating minimal repunits that divide by n.
{-
A number consisting entirely of ones is called a repunit. We shall define R(k)
to be a repunit of length k; for example, R(6) = 111111.

Given that n is a positive integer and GCD(n, 10) = 1, it can be shown that
there always exists a value, k, for which R(k) is divisible by n, and let A(n)
be the least such value of k; for example, A(7) = 6 and A(41) = 5.

The least value of n for which A(n) first exceeds ten is 17.

Find the least value of n for which A(n) first exceeds one-million.
-}

{-
n | R(k)
n | (10^k - 1)/9
9n | (10^k - 1)
10^k - 1 == 0 (mod 9n)
10^k == 1 (mod 9n)

A(n) = multiplicative order of 10, mod 9n.
-}

-- a n = minimal k such that n divides R(k)
a n = mult_order 10 (9*n)

prob129 m = head $ filter (\n -> m < a n) [m ..]

main :: IO String
main = return $ show $ prob129 1000000
--- 1000023

---------------------------------------

repunit k = (10^k - 1) `div` 9

funExp f n x = g n x
  where
    g 0 x = x
    g n x = g (n-1) $! (f x)

repunitMod k n = funExp f k 0
  where f x = (10 * x + 1) `mod` n

