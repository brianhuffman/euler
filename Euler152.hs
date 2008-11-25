module Euler152 where
import Permutation
import EulerLib (divides)
import Primes
import Data.Ratio
import Data.List

{-
Problem 152
Writing 1/2 as a sum of inverse squares

27 April 2007

There are several ways to write the number 1/2 as a sum of inverse squares
using distinct integers.

For instance, the numbers {2,3,4,5,7,12,15,20,28,35} can be used:

1/2 = sum $ map (\x -> 1/x^2) [2,3,4,5,7,12,15,20,28,35]

In fact, only using integers between 2 and 45 inclusive, there are exactly
three ways to do it, the remaining two being: {2,3,4,6,7,9,10,20,28,35,36,45}
and {2,3,4,6,7,9,12,15,28,30,35,36,45}.

How many ways are there to write the number 1/2 as a sum of inverse squares
using distinct integers between 2 and 80 inclusive?
-}

frac n = 1 % (n^2)

f :: (Integral a) => [a] -> [a] -> Ratio a -> [[a]]
f [] [] x = if x == 0 then [[]] else []
f [] (d:ds) x = f [] ds x ++
  [ d:ds' |
    let x' = x - frac d,
    x' >= 0,
    x' <= sum (map frac ds),
    ds' <- f [] ds x' ]
f (p:ps) ds x =
  [ ds1' ++ ds2' |
    let (ds1, ds2) = partition (divides p) ds,
    ds1' <- subsets ds1,
    let x' = x - sum (map frac ds1'),
    not (p `divides` denominator x'),
    x' >= 0,
    ds2' <- f ps ds2 x' ]

prob152 m = f ps [2 .. m] (1%2)
  where
    -- factors that should not appear in the denominator of the sum
    ps = reverse $ takeWhile (<=m) $ drop 1 primes

main :: IO String
main = return $ show $ length $ prob152 80
-- 301
