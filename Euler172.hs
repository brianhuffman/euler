module Euler172 where
import EulerLib
import Array

------------------------------------------------------------------------------
-- 172. Investigating numbers with few repeated digits.
-- How many 18-digit numbers with no digit occurring more than 3 times?

prob172a = a
  where
    -- a!(n,d) = how many n-length using digits d through 9 ?
    a = funArray ((0,0),(18,9)) f
    f (n,9) = if n <= 3 then 1 else 0
    f (0,d) = 1
    f (n,0) = sum [ a!(n-r, 1) * choose (n-1) r | r <- [0 .. min n 3] ]
    f (n,d) = sum [ a!(n-r, d+1) * choose n r | r <- [0 .. min n 3] ]

main :: IO String
main = return $ show $ prob172a!(18,0)
-- 227485267000992000
