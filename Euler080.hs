module Euler080 where
import SquareRoot
import qualified SortedList as S
import Data.Char (digitToInt)

------------------------------------------------------------------------------
-- 80. Calculating the digital sum of the decimal digits of irrational square roots.
{-
It is well known that if the square root of a natural number is not an integer,
then it is irrational. The decimal expansion of such square roots is infinite
without any repeating pattern at all.

The square root of two is 1.41421356237309504880..., and the digital sum of the
first one hundred decimal digits is 475.

For the first one hundred natural numbers, find the total of the digital sums
of the first one hundred decimal digits for all the irrational square roots.
-}

prob80 = sum (map f ns)
  where
    f n = sum $ map digitToInt $ take 100 $ show $ square_root (n*10^200)
    ns = S.deleteFirsts [1..100] (map (^2) [1..10])
-- 40886

main :: IO String
main = return $ show $ prob80
