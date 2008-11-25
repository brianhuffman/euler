module Euler007 where
import Primes

------------------------------------------------------------------------------
-- 7. Find the 10001st prime.

-- first prime is primes !! 0
prob7 :: Int -> Int
prob7 n = primes !! (n - 1)
-- prob7 10001 = 104743

main :: IO String
main = return $ show $ prob7 10001