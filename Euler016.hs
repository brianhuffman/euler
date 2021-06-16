module Euler016 where
import EulerLib
import Data.Char

------------------------------------------------------------------------------
-- 16. What is the sum of the digits of the number 2^1000?

prob16 :: Int -> Int
prob16 n = sum' $ map digitToInt $ show $ (2::Integer) ^ n
-- prob16 1000 = 1366

main :: IO String
main = return $ show $ prob16 1000
