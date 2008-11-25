module Euler015 where
import EulerLib

------------------------------------------------------------------------------
-- 15. Starting in the top left corner in a 20 by 20 grid, how many routes are there to the bottom right corner?

prob15 :: Int -> Int -> Integer
prob15 m n = (m+n) `choose` m
-- prob15 20 20 = 137846528820

main :: IO String
main = return $ show $ prob15 20 20