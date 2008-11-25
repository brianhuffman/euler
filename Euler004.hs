module Euler004 where

------------------------------------------------------------------------------
-- 4. Find the largest palindrome made from the product of two 3-digit numbers.

palindrome s = s == reverse s

prob4 =
  maximum $
  filter (palindrome . show) $
  [x * y | x <- [100 .. 999], y <-[100 .. 999]]
-- 906609 = 913 * 993

prob4a n =
  -- maximum $
  filter (palindrome . show . fst) $
  [ (x * y, (x, y)) | x <- xs, y <- ys x]
  where
    ys x = [10^n - 1, 10^n - 2 .. x]
    xs = ys (10^(n-1))
-- 906609

prob4b n = head
  [ (p, y) |
    x <- [10^n - 1, 10^n - 2 .. 1],
    let p = mkpal x,
    y <- [10^n - 1, 10^n - 2 .. x+1],
    p `mod` y == 0 ]
  where
    mkpal x = let s = show x in read (s ++ reverse s)
-- 9009 = 91 * 99
-- 906609 = 913 * 993
-- 99000099 = 9901 * 9999
-- 9966006699 = 99681 * 99979
-- 999000000999 = 999001 * 999999
-- 99956644665999 = 9998017 * 997647
-- 9999000000009999 = 99990001 * 99999999

main :: IO String
main = return $ show $ fst $ prob4b 3
