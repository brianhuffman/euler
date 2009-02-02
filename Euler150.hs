module Euler150 where
import Data.Int
import Data.Array.Unboxed
import Control.Monad.ST
import Data.Array.ST
import Data.Array.IArray
import Control.Monad (foldM)

{-
Problem 150
Searching a triangular array for a sub-triangle having minimum-sum.
13 April 2007

In a triangular array of positive and negative integers, we wish to
find a sub-triangle such that the sum of the numbers it contains is
the smallest possible.

In the example below, it can be easily verified that the marked
triangle satisfies this condition having a sum of -42.

          15
            / \
       -14 /-7 \
          /     \
      20 /-13 -5 \
        /         \
    -3 / 8  23 -26 \
      /             \
   1 / -4  -5 -18  5 \
     -----------------
-16  31   2   9  28  3

We wish to make such a triangular array with one thousand rows, so we
generate 500500 pseudo-random numbers s(k) in the range 219, using a
type of random number generator (known as a Linear Congruential
Generator) as follows:

  t := 0
  for k = 1 up to k = 500500:
    t := (615949*t + 797807) modulo 220
    s(k) := t - 2^19

Thus: s(1) = 273519, s(2) = 153582, s(3) = 450905 etc

Our triangular array is then formed using the pseudo-random numbers
thus:

      s1
    s2  s3
  s4  s5  s6 
s7  s8  s9  s10
     ...

Sub-triangles can start at any element of the array and extend down as
far as we like (taking-in the two elements directly below it from the
next row, the three elements directly below from the row after that,
and so on). The "sum of a sub-triangle" is defined as the sum of all
the elements it contains.

Find the smallest possible sub-triangle sum.
-}

-- linear congruential generator
lcg :: () -> [Int]
lcg () = tail (f 0)
  where f t = t - 2^19 : f ((615949*t + 797807) `mod` 2^20)

{-
Any algorithm for this will take O(n^3) time.

Is it possible to take O(n^2) space?

Is it possible to take O(n) space?

Coordinates:
We can describe any location in a triangle with two coordinates (x, y).
Starting from the top, move down-left x times, and down-right y times.
The location (x, y) is found on row (x + y).

We can describe any sub-triangle with three coordinates (x, y, z).
The apex is at (x, y), and the base is z rows down.
Thus the bottom corners are at (x+h, y) and (x, y+h).
The base of the triangle is found at row (x + y + h).

Define T(x,y) = value at position (x,y).
Define S(x,y,h) = sum of sub-triangle (x,y,h).

S(x, y, 0) = T(x, y)
S(x, y, 1) = T(x, y) + T(x+1, y) + T(x, y+1)
S(x, y, h+2) = T(x, y) + S(x+1, y, h+1) + S(x, y+1, h+1) - S(x+1, y+1, h)

S(x, y, h) = T(x, y+h) + S(x, y, h-1) + S(x+1, y, h-1) - S(x+1, y, h-2)
-}

type Z = Int

random_triangle :: Int -> [[Z]]
random_triangle n = take n $ g 1 $ map fromIntegral $ lcg ()
  where
    g n xs = let (ys,zs) = splitAt n xs in ys : g (n+1) zs

test_triangle :: [[Z]]
test_triangle =
  [[15],
   [-14,-7],
   [20,-13,-5],
   [-3,8,23,-26],
   [1,-4,-5,-18,5],
   [-16,31,2,9,28,3]]

prep_triangle :: [[Z]] -> [UArray Int Z]
prep_triangle = zipWith prep_row [1..]
  where
    prep_row n xs = listArray (0, n) (scanl (+) 0 xs)

find :: Int -> [UArray Int Z] -> Z
find w [] = 0
find w arrs = min z1 z2
  where
    z1 = minimum [ find1 x arrs | x <- [1 .. w] ]
    z2 = find (w+1) (tail arrs)

find1 :: Int -> [UArray Int Z] -> Z
find1 x arrs = min_init_sum rows
--find1 x arrs = minimum (scanl (+) 0 rows)
  where
    rows = zipWith (\y a -> a!y - a!(x-1)) [x..] arrs

-- min_init_sum xs = minimum (scanl (+) 0 xs)
min_init_sum :: [Z] -> Z
min_init_sum = f 0 0
  where
    f z t [] = z
    f z t (x : xs) = f z' t' xs
      where
        t' = t + x
        z' = min z t'

prob150 :: Int -> Z
prob150 n = find 1 (prep_triangle (random_triangle n))

main :: IO String
main = return $ show $ prob150 1000

answer :: String
answer = "-271248680"
