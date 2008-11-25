module Euler040 where
import Char

------------------------------------------------------------------------------
-- 40. Finding the nth digit of the fractional part of the irrational number.

main :: IO String
main = return $ show $ product $ map (digitToInt . (d !!)) ns
  where
    d = concatMap show [0 ..]
    ns = [1,10,100,1000,10000,100000,1000000]
-- 210

