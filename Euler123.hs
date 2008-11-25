module Euler123 where
import Primes

------------------------------------------------------------------------------
-- 123. Determining the remainder when (p_n − 1)^n + (p_n + 1)^n is divided by p_n^2.
{-
Let p_n be the nth prime: 2, 3, 5, 7, 11, ..., and let r be the remainder when
(p_n - 1)^n + (p_n + 1)^n is divided by (p_n)^2.

For example, when n = 3, p_3 = 5, and 4^3 + 6^3 = 280 == 5 mod 25.

The least value of n for which the remainder first exceeds 10^9 is 7037.

Find the least value of n for which the remainder first exceeds 10^10.
-}

{-
Use binomial expansion:
(a + b)^n == n*a*b^(n-1) + b^n  (mod a^2)

(a+1)^n == n*a + 1  (mod a^2)
(a-1)^n == 1 - n*a  (mod a^2)  (for even n)
(a-1)^n == n*a - 1  (mod a^2)  (for odd n)
(a+1)^n + (a-1)^n == 2      (mod a^2)  (for even n)
(a+1)^n + (a-1)^n == 2*n*a  (mod a^2)  (for odd n)
-}

prob123 m = head
  [ n |
    (p, n) <- zip primes [1 ..],
    odd n,
    (2*n*p) `mod` (p^2) > m ]

main :: IO String
main = return $ show $ prob123 (10^10)
-- 21035
