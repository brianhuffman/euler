module Euler048 where
import Primes

------------------------------------------------------------------------------
-- 48. Find the last ten digits of 1^1 + 2^2 + ... + 1000^1000.

prob48 m = sum [ expMod a a n | a <- [1 .. m] ] `mod` n
  where n = 10 ^ 10

main :: IO String
main = return $ show $ prob48 1000
-- 9110846700

