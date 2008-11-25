module Euler010 where
import PrimeArray (primes_upto)

------------------------------------------------------------------------------
-- 10. Calculate the sum of all the primes below two million.

prob10 :: Int -> Integer
prob10 m = sum $ map toInteger $ primes_upto (m-1)

main :: IO String
main = return $ show $ prob10 (2*10^6)
-- 142913828922
