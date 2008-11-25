module Euler001 where

------------------------------------------------------------------------------
-- 1. Add all the natural numbers below 1000 that are multiples of 3 or 5.

divides x y = y `mod` x == 0
prob1 = sum (filter (\x -> divides 3 x || divides 5 x) [1..999])
-- 233168


-- more efficient version
triangle x = x * (x + 1) `div` 2

prob1a n = f 3 + f 5 - f 15
  where f k = k * triangle ((n - 1) `div` k)

main :: IO String
main = return $ show $ prob1a 1000
