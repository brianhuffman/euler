module Euler005 where

------------------------------------------------------------------------------
-- 5. What is the smallest number divisible by each of the numbers 1 to 20?

prob5 :: Integer -> Integer
prob5 n = foldr lcm 1 [1 .. n]
-- prob5 20 = 232792560

main :: IO String
main = return $ show $ prob5 20