module Euler069 where
import Primes

------------------------------------------------------------------------------
-- 69. Find the value of n ≤ 1,000,000 for which n/φ(n) is a maximum.

-- f(n) = n/phi(n)
-- for prime p, f(p) = p/(p-1)
-- for prime p, f(p^n) = p/(p-1)
-- f is multiplicative: for relatively prime p q, f(p*q) = f(p) * f(q)

-- product of as many small primes as possible
prob69 :: Int -> Int
prob69 n = last $ takeWhile (<= n) $ scanl1 (*) primes

main :: IO String
main = return $ show $ prob69 (10^6)
-- 510510

