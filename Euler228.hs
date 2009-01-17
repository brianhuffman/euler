module Euler228 where
import Primes
-- import Data.Ratio
-- import qualified SortedList as S

{-
Problem 228
17 January 2009

Let S_(n) be the regular n-sided polygon – or shape – whose vertices
v_(k) (k = 1,2,…,n) have coordinates:

	x_(k)   =   cos( ^(2k-1)/_(n) ×180° )
	y_(k)   =   sin( ^(2k-1)/_(n) ×180° )

Each S_(n) is to be interpreted as a filled shape consisting of all
points on the perimeter and in the interior.

The Minkowski sum, S+T, of two shapes S and T is the result of adding
every point in S to every point in T, where point addition is
performed coordinate-wise: (u, v) + (x, y) = (u+x, v+y).

For example, the sum of S_(3) and S_(4) is the six-sided shape shown
in pink below:

picture showing S_3 + S_4

How many sides does S_(1864) + S_(1865) + … + S_(1909) have?

-}

{-
The Minkowski sum has one edge for each distinct slope any of
the summand polygons have.

The n-sided polygon has n slopes 1/n, 2/n ... n/n.

For edges on two different polygons to have the same slope,
their edge numbers must have a common divisor.
-}

-- 86226

prob228 :: (Int, Int) -> Int
prob228 (m, n) = sum [m..n] - sum (map dups ds)
  where
    ds = [1 .. n-m]
    mults d = n `div` d - (m-1) `div` d
    dups d = totient d * (mults d - 1)

main :: IO String
main = return $ show $ prob228 (1864, 1909)

{-
length [1864 .. 1909] = 46
sum [1864 .. 1909] = 86779
k/1 appears 46 times.
k/2 appears 23 times.
k/3 appears 15 times.
k/4 appears 12 times.
...
-}

{-
angles :: Integer -> [Rational]
angles n = [ k % n | k <- [1 .. n] ]

prob228 :: [Integer] -> Int
prob228 ns = length $ foldl1 S.union $ map angles ns

main :: IO String
main = return $ show $ prob228 [1864 .. 1909]
-}
