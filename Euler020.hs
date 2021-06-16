module Euler020 where
import EulerLib
import Data.Char

------------------------------------------------------------------------------
-- 20. Find the sum of digits in 100!

prob20 :: Integer -> Int
prob20 n = sum' $ map digitToInt $ show $ product [1 .. n]

main :: IO String
main = return $ show $ prob20 100
-- 648
