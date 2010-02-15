module Euler270 where
import EulerLib (funArray)
import Data.Array

{---------------------------------------------------------------------
Problem 270
Cutting Squares

26 December 2009

A square piece of paper with integer dimensions N×N is placed with a
corner at the origin and two of its sides along the x- and
y-axes. Then, we cut it up respecting the following rules:

    * We only make straight cuts between two points lying on different
      sides of the square, and having integer coordinates.

    * Two cuts cannot cross, but several cuts can meet at the same
      border point.

    * Proceed until no more legal cuts can be made.

Counting any reflections or rotations as distinct, we call C(N) the
number of ways to cut an N×N square. For example, C(1) = 2 and
C(2) = 30 (shown below).

What is C(30) mod 10^(8) ?

---------------------------------------------------------------------}

prob270 :: Int -> Integer
prob270 n = f5
  where
    x ** y = x * y
    a2 = funArray ((0,0), (n,n)) f2
    f2 (i,j) | i > j = a2!(j,i)
    f2 (0,j) = 0
    f2 (1,1) = 1
    f2 (i,j) = (a2!(i-1,j)) + (a2!(i,j-1))
    a3 = funArray ((0,0), (n,n)) f3
    f3 (i,j) | i > j = f3 (j,i)
    f3 (0,j) = a2!(n,j)
    f3 (i,j) = (a3!(i-1,j)) + (a3!(i,j-1))
      + sum [ (a2!(i,k)) ** (a2!(n-k,j)) | k <- [1 .. n-1] ]
    a4 = funArray ((0,0), (n,n)) f4
    f4 (i,j) | i > j = f4 (j,i)
    f4 (0,j) = a3!(n,j)
    f4 (i,j) = (a4!(i-1,j)) + (a4!(i,j-1))
      + sum [ (a2!(i,k)) ** (a3!(n-k,j)) | k <- [1 .. n-1] ]
      + (a2!(i,n)) ** (a2!(n,j))
      + sum [ (a3!(i,k)) ** (a2!(n-k,j)) | k <- [1 .. n-1] ]
    f5 = a4!(n-1,n-1)
      + sum [ (a2!(n,k)) ** (a3!(n-k,n-1)) | k <- [1 .. n-1] ]
      + (a2!(n,n)) ** (a2!(n,n-1))
      + sum [ (a3!(n,k)) ** (a2!(n-k,n-1)) | k <- [1 .. n-1] ]

main :: IO String
main = return $ show $ prob270 30 `mod` (10^8)

answer :: String
answer = "82282080"
