module Euler003 where
import Primes

------------------------------------------------------------------------------
-- 3. Find the largest prime factor of 317584931803.

n1 = 317584931803
n2 = 600851475143

prob3 :: Integer -> Integer
prob3 n = fst $ last $ prime_factorization n
-- prob3 317584931803 = 3919
-- prob3 600851475143 = 6857

main :: IO String
main = return $ show $ prob3 600851475143
