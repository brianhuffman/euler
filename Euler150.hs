module Euler150 where
import Data.List
import Data.Int
--import Data.Array.Unboxed
import Control.Monad.ST
import Data.Array.ST
import Data.Array
import Control.Monad (foldM)

{-
Problem 150
Searching a triangular array for a sub-triangle having minimum-sum.
13 April 2007

In a triangular array of positive and negative integers, we wish to find a
sub-triangle such that the sum of the numbers it contains is the smallest
possible.

In the example below, it can be easily verified that the marked triangle
satisfies this condition having a sum of -42.

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

We wish to make such a triangular array with one thousand rows, so we generate
500500 pseudo-random numbers s(k) in the range 219, using a type of random
number generator (known as a Linear Congruential Generator) as follows:

  t := 0
  for k = 1 up to k = 500500:
    t := (615949*t + 797807) modulo 220
    s(k) := t - 2^19

Thus: s(1) = 273519, s(2) = 153582, s(3) = 450905 etc

Our triangular array is then formed using the pseudo-random numbers thus:

      s1
    s2  s3
  s4  s5  s6 
s7  s8  s9  s10
     ...

Sub-triangles can start at any element of the array and extend down as far as
we like (taking-in the two elements directly below it from the next row, the
three elements directly below from the row after that, and so on). The "sum
of a sub-triangle" is defined as the sum of all the elements it contains.

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

tminimum :: [[Z]] -> Z
tminimum = minimum . map minimum

tplus :: [[Z]] -> [[Z]] -> [[Z]]
tplus = zipWith (zipWith (+))

{-
prob150b :: [[Z]] -> [Z]
prob150b t0 = f (tminimum t0) t0 t0
  where
    f m l [] = []
    f m l t = m : f m' l' t'
      where
         l' = tplus t0 (tail l)
         t' = tplus l' (map tail (tail t))
         m' = min m (tminimum t')
-}

{-
prob150e t0 = map f (tails (reverse t0))
  where
    f [] = 0
    f (xs:xss) = g (minimum xs) (repeat 0) xs xss
    total x l r z = x + l + r - z
    g m zs ys [] = m
    g m zs ys (xs:xss) = (g $! m') ys ys' xss
      where
        ys' = zipWith4 total xs ys (tail ys) (tail zs)
        m' = minimum (m : ys')
-}

sums_triangle :: [Z] -> [[Z]]
sums_triangle xs = f xs xs
  where
    f xs [] = []
    f xs (_:ys) = xs : f (zipWith (+) xs ys) ys

min_subtriangle_sum :: [[Z]] -> Z
min_subtriangle_sum t0 = fst $ foldl f (0, []) t0
  where
    f (z,yss) xs = (z', yss')
      where
        t = sums_triangle xs
        yss' = xs : tplus yss (tail t)
        z' = minimum (z : map minimum yss')
prob150 :: Int -> Z
prob150 n = min_subtriangle_sum (random_triangle n)

main :: IO String
main = return $ show $ prob150 1000
-- -271248680





-- solution using unboxed arrays
{-
rand_triangle :: Int -> [Row]
rand_triangle n = reverse $ take n $ g 1 $ map fromIntegral $ lcg ()
  where
    g n xs = listArray (1, n) ys : g (n+1) zs
      where
        (ys, zs) = splitAt n xs



funArray :: (IArray a e, Ix i) => (i, i) -> (i -> e) -> a i e
funArray ij f = listArray ij (map f (range ij))

type Row = UArray Int Z

-- lists sorted with bottom of triangle first

rand_triangle :: Int -> [Row]
rand_triangle n = reverse $ take n $ g 1 $ map fromIntegral $ lcg ()
  where
    g n xs = listArray (1, n) ys : g (n+1) zs
      where
        (ys, zs) = splitAt n xs

row_min :: Row -> Z
row_min = minimum . elems

rows_min :: [Row] -> Z
rows_min = minimum . map row_min

more_sums :: [Row] -> Row -> [Row]
more_sums oldsums r = f (snd (bounds r)) (repeat 0) s0 oldsums
  where
    s0 = r
    xs0 = elems s0
    -- precondition: (_, n) = bounds s
    f 1 xs s _ = [s]
    f n xs s (r:rs) = s : f n' xs' s'
      where
        n' = n - 1
        ys = elems s
        xs' = tail ys
        s' = listArray (1, n-1) (zipWith (-) (zipWith (+) ys xs')
    s1 = listArray (1, n-1) (zipWith (+) xs0 (tail xs0))
    f s1 s2 [] = [s1, s2]
-- next_sum 
-}