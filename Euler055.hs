module Euler055 where
import EulerLib

------------------------------------------------------------------------------
-- 55. How many Lychrel numbers are there below ten-thousand?

lychrel 0 n = True
lychrel k n = not (palindrome (show n')) && lychrel (k-1) n'
  where n' = n + read (reverse (show n))

prob55 k n = filter (lychrel k) [1 .. n-1]

main :: IO String
main = return $ show $ length $ prob55 50 10000
-- 249

