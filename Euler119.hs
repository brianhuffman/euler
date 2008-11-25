module Euler119 where
import EulerLib
import qualified SortedList as S
import Char (digitToInt)

------------------------------------------------------------------------------
-- 119. Investigating the numbers which are equal to sum of their digits raised to some power.
{-
The number 512 is interesting because it is equal to the sum of its digits
raised to some power: 5 + 1 + 2 = 8, and 8^3 = 512. Another example of a number
with this property is 614656 = 28^4.

We shall define a(n) to be the nth term of this sequence and insist that a
number must contain at least two digits to have a sum.

You are given that a(2) = 512 and a(10) = 614656.

Find a(30).
-}

sumdigits :: Integer -> Integer
sumdigits = sum . map (fromIntegral . digitToInt) . show

powers :: [(Integer, Integer, Integer)]
powers = S.big_union
  [ [ (n^k, n, k) | n <- [2 .. 10^k] ] | k <- [2 ..] ]

prob119 :: [Integer]
prob119 = [ x | (x,n,k) <- powers, sumdigits x == n ]

main :: IO String
main = return $ show $ prob119 !! (30-1)
-- 248155780267521
