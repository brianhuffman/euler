module Euler181 where
import EulerLib
import Array
import Data.Int

{-
Problem 181
09 February 2008

Having three black objects B and one white object W they can be grouped
in 7 ways like this:

  (BBBW)  (B,BBW)  (B,B,BW)  (B,B,B,W)  (B,BB,W)  (BBB,W)  (BB,BW)

In how many ways can sixty black objects B and forty white objects W be
thus grouped?
-}

{-
See also Problem 78.

p(n) = number of partitions of n

Define g(m,n) = number of partitions of n
  which only contain elements from [1 .. m]

Recursion equations:

For n=0, only the empty partition works.
  g(0, m) = 1

For n/=0 and m=0, no partitions are possible.
  g(n, 0) = 0

In other cases, the partition either starts with m, or doesn't.
  g(n, m)
    | m <= n    = g(n-m, m) + g(n, pred m)
    | otherwise = g(n, pred m)
-}

type Z = Int64
type Pair = (Int, Int)

p2_array :: Pair -> Array Pair (Array Pair Z)
p2_array (xmax, ymax) = a
  where
    a = funArray ((0, 0), (xmax, ymax)) f
    f (x, y) = funArray ((0, 0), (x, y)) (g (x, y))
    -- precondition: i<=x, j<=y
    g (0, 0) (i, j) = 1
    g (x, y) (0, 0) = 0
    g (x, y) (i, j) = a!(x', y')!(i', j') + a!(x, y)!(i2, j2)
      where
        (x', y') = (x-i, y-j)
        (i', j') = if i > x' then (x', y') else (i, min j y')
        (i2, j2) = if j == 0 then (i-1, y) else (i, j-1)

prob181 :: Int -> Int -> Z
prob181 x y = a ! (x, y) ! (x, y)
  where a = p2_array (x, y)

main :: IO String
main = return $ show $ prob181 60 40
-- 83735848679360680
