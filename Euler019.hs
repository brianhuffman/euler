module Euler019 where

divides x y = y `mod` x == 0

------------------------------------------------------------------------------
-- 19. How many Sundays fell on the first of the month during the twentieth century?

leap_year n =
  4 `divides` n && (not (100 `divides` n) || 400 `divides` n)

month_lengths n =
  [31,if leap_year n then 29 else 28,31,30,31,30,31,31,30,31,30,31]

main :: IO String
main = return $ show $
  length $
  filter (divides 7) $
  take 1200 $
  scanl (+) 3 $
  concatMap month_lengths [1901 ..]
-- 171
