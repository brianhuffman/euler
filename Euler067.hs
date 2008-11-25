module Euler067 where

------------------------------------------------------------------------------
-- 67. Using an efficient algorithm find the maximal sum in the triangle?
{-
By starting at the top of the triangle below and moving to adjacent numbers on
the row below, the maximum total from top to bottom is 23.

   3
  7 5
 2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom in triangle.txt, a 15K text file
containing a triangle with one-hundred rows.

NOTE: This is a much more difficult version of Problem 18. It is not possible
to try every route to solve this problem, as there are 299 altogether! If you
could check one trillion (1012) routes every second it would take over twenty
billion years to check them all. There is an efficient algorithm to solve it.
;o)
-}

triangleTxt :: IO [[Int]]
triangleTxt =
  readFile "triangle.txt" >>= return . map (map read . words) . lines

prob18a :: [Int]  -> [[Int]] -> [Int]
prob18a vs [] = vs
prob18a vs (xs : xss) = prob18a ws xss
  where
    ys = zipWith (+) xs ([0] ++ vs)
    zs = zipWith (+) xs (vs ++ [0])
    ws = zipWith max ys zs

prob67a :: [[Int]] -> Int
prob67a xss = maximum $ prob18a [] xss

main :: IO String
main = triangleTxt >>= return . show . prob67a
-- 7273