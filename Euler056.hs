module Euler056 where
import Char (digitToInt)

------------------------------------------------------------------------------
-- 56. Considering natural numbers of the form, a^b, finding the maximum digital sum.

digital_sum :: Integer -> Int
digital_sum = sum . map digitToInt . show

prob56 :: Integer -> Int
prob56 n = maximum
  [ digital_sum (a^b) | a <- [1 .. n-1], b <- [1 .. n-1] ]

main :: IO String
main = return $ show $ prob56 100
-- 972

