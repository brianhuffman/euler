module Euler130 where
import EulerLib
import Primes

------------------------------------------------------------------------------
-- 130. Finding composite values, n, for which n−1 is divisible by the length of the smallest repunits that divide it.

{-
A number consisting entirely of ones is called a repunit. We shall define
R(k) to be a repunit of length k; for example, R(6) = 111111.

Given that n is a positive integer and GCD(n, 10) = 1, it can be shown that
there always exists a value, k, for which R(k) is divisible by n, and let
A(n) be the least such value of k; for example, A(7) = 6 and A(41) = 5.

You are given that for all primes, p > 5, that p - 1 is divisible by A(p).
For example, when p = 41, A(41) = 5, and 40 is divisible by 5.

However, there are rare composite values for which this is also true; the
first five examples being 91, 259, 451, 481, and 703.

Find the sum of the first twenty-five composite values of n for which
GCD(n, 10) = 1 and n - 1 is divisible by A(n).
-}

-- From Euler129.hs
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

prob130 =
  [ n | n <- [3, 5 ..], not (is_prime n), gcd n 10 == 1,
    divides (a n) (n-1) ]

main :: IO String
main = return $ show $ sum $ take 25 prob130
-- 149253

