module Euler149 where
import EulerLib (chunk)
import Lagged

{-
Problem 149
Searching for a maximum-sum subsequence.

13 April 2007

Looking at the table below, it is easy to verify that the maximum possible
sum of adjacent numbers in any direction (horizontal, vertical, diagonal or
anti-diagonal) is 16 (= 8 + 7 + 1).

 -2   5   3   2
  9  -6   5   1
  3   2   7   3
 -1   8  -4   8

Now, let us repeat the search, but on a much larger scale:

First, generate four million pseudo-random numbers using a specific form of
what is known as a "Lagged Fibonacci Generator":

For 1 <= k <= 55,
s(k) = [100003 - 200003k + 300007k^3] (modulo 1000000) - 500000.
For 56 <= k <= 4000000,
s(k) = [s(k-24) + s(k-55) + 1000000] (modulo 1000000) - 500000.

Thus, s(10) = -393027 and s(100) = 86613.

The terms of s are then arranged in a 2000x2000 table, using the first 2000
numbers to fill the first row (sequentially), the next 2000 numbers to fill
the second row, and so on.

Finally, find the greatest sum of (any number of) adjacent entries in any
direction (horizontal, vertical, diagonal or anti-diagonal).
-}

type Rect = [[Int]]

random_rect :: Int -> Rect
random_rect n =
  take n $ chunk n $ map (subtract 500000) $ lagged_fibonacci

max0sum :: Int -> Int -> Int
max0sum m x = max 0 (m+x)

max_subsequence_sum :: [Int] -> Int
max_subsequence_sum = foldl max 0 . scanl max0sum 0

test_rectangle = [[-2,5,3,2],[9,-6,5,1],[3,2,7,3],[-1,8,-4,8]]

prob149_horiz :: Rect -> Int
prob149_horiz xss =
  maximum $
  map max_subsequence_sum $
  xss
-- 47107641

prob149_vert :: Rect -> Int
prob149_vert xss =
  maximum $
  foldl (zipWith max) (repeat 0) $
  scanl (zipWith max0sum) (repeat 0) $
  xss
-- 52852124

prob149_diag1 :: Rect -> Int
prob149_diag1 xss =
  maximum $
  foldl (zipWith max) (repeat 0) $
  scanl (\ms xs -> zipWith max0sum (0:ms) xs) (repeat 0) $
  xss
-- 43441203

prob149_diag2 :: Rect -> Int
prob149_diag2 xss =
  maximum $
  foldl (zipWith max) (repeat 0) $
  scanl (\ms xs -> zipWith max0sum (tail ms ++ [0]) xs) (repeat 0) $
  xss
-- 42783189

prob149_all :: Rect -> Int
prob149_all xss = f 0 m0 m0 m0 xss
  where
    m0 = repeat 0
    f z us vs ws [] = z
    f z us vs ws (xs:xss) = (f $! z') us' vs' ws' xss
      where
        us' = zipWith max0sum xs (0 : us)
        vs' = zipWith max0sum xs vs
        ws' = zipWith max0sum xs (tail ws ++ [0])
        u = maximum us'
        v = maximum vs'
        w = maximum ws'
        x = max_subsequence_sum xs
        z' = maximum [u,v,w,x,z]

prob149 :: Int -> Int
prob149 n = prob149_all (random_rect n)

main :: IO String
main = return $ show $ prob149 2000
-- 52852124

-- TODO: rewrite using STUArray
