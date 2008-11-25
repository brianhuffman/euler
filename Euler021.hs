module Euler021 where
import EulerLib
import Primes

------------------------------------------------------------------------------
-- 21. Evaluate the sum of all amicable pairs under 10000.

sum_proper_factors x = sum_of_divisors x - x

amicable x = x /= y && x == z
  where
    y = sum_proper_factors x
    z = sum_proper_factors y

amicables_below n = filter amicable [2 .. n-1]

prob21 :: Int -> Int
prob21 n = sum' (amicables_below n)
-- prob21 10000 = 31626
-- prob21 100000 = 852810  (15 seconds)

main :: IO String
main = return $ show $ prob21 10000
-- 31626