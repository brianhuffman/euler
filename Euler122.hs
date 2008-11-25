module Euler122 where
import qualified SortedList as S
import List

------------------------------------------------------------------------------
-- 122. Finding the most efficient exponentiation method.
{-
The most naive way of computing n^15 requires fourteen multiplications:

n * n * ... * n = n^15

But using a "binary" method you can compute it in six multiplications:

n    * n   = n^2
n^2  * n^2 = n^4
n^4  * n^4 = n^8
n^8  * n^4 = n^12
n^12 * n^2 = n^14
n^14 * n   = n^15

However it is yet possible to compute it in only five multiplications:

n *  * n   = n^2
n^2  * n   = n^3
n^3  * n^3 = n^6
n^6  * n^6 = n^12
n^12 * n^3 = n^15

We shall define m(k) to be the minimum number of multiplications to compute
n^k; for example m(15) = 5.

For 1 <= k <= 200, find the sum of m(k).
-}

-- http://www.research.att.com/~njas/sequences/A003313

-- addition_chains !! n = all addition chains of length n
addition_chains :: [[[Int]]]
addition_chains = map (map (map negate)) $ iterate f [[-1]]
  where
    f xss =
      [ x : xs |
        xs <- xss,
        x <- S.big_union (map (zipWith (+) xs) (tails xs)),
        x < head xs,
        x >= -200 ]

rcompare :: Int -> Int -> Ordering
rcompare x y = compare y x

rinsert :: Int -> [Int] -> [Int]
rinsert = S.insertBy rcompare

runion :: [Int] -> [Int] -> [Int]
runion = S.unionBy rcompare

-- In k steps, we can get at most 2^k.
too_low :: Int -> [Int] -> Bool
too_low k [] = False
too_low k (x:xs) = 2^k < x || too_low (k-1) xs

-- add_chain k xs =
-- addition chains containing xs, with length at most k.
-- precondition: xs is reverse-sorted.
add_chain :: Int -> [Int] -> [[Int]]
add_chain k [1] = [[1]]
add_chain k (x:xs)
  | too_low k (x:xs) = []
  | has_sum x xs = map (x :) (add_chain (k-1) xs)
  | x < 2 * max_x = map (x :) (add_chain (k-1) (rinsert (x - max_x) xs))
  | otherwise =
    [ x : ts |
      y2 <- [(x+1)`div`2 .. x-1],
      let y1 = x - y2,
      let ys = if y1 == y2 then [y1] else [y2,y1],
      ts <- add_chain (k-1) (runion ys xs)
    ]
  where
    max_x = if null xs then 1 else head xs
    has_sum t [] = False
    has_sum t (x:xs)
      | x == t-1 = True
      | otherwise = S.elemBy rcompare (t-x) xs || has_sum t xs

shortest_chain :: Int -> Int
shortest_chain x = f 0
  where
    f k = if null (add_chain k [x]) then f (k+1) else k

prob122 :: Int -> Int
prob122 n = sum (map shortest_chain [1 .. n])

main :: IO String
main = return $ show $ prob122 200
-- 1582

{-
map shortest_chain [1 .. 200] =
  [  0,  1,  2,  2,  3,  3,  4,  3,  4,  4  -- 10
  ,  5,  4,  5,  5,  5,  4,  5,  5,  6,  5  -- 20
  ,  6,  6,  6,  5,  6,  6,  6,  6,  7,  6  -- 30
  ,  7,  5,  6,  6,  7,  6,  7,  7,  7,  6  -- 40
  ,  7,  7,  7,  7,  7,  7,  8,  6,  7,  7  -- 50
  ,  7,  7,  8,  7,  8,  7,  8,  8,  8,  7  -- 60
  ,  8,  8,  8,  6,  7,  7,  8,  7,  8,  8  -- 70
  ,  9,  7,  8,  8,  8,  8,  8,  8,  9,  7  -- 80
  ,  8,  8,  8,  8,  8,  8,  9,  8,  9,  8  -- 90
  ,  9,  8,  9,  9,  9,  7,  8,  8,  8,  8  -- 100
  ,  9,  8,  9,  8,  9,  9,  9,  8,  9,  9  -- 110
  ,  9,  8,  9,  9,  9,  9,  9,  9,  9,  8  -- 120
  ,  9,  9,  9,  9,  9,  9, 10,  7,  8,  8  -- 130
  ,  9,  8,  9,  9,  9,  8,  9,  9, 10,  9  -- 140
  , 10, 10, 10,  8,  9,  9,  9,  9,  9,  9  -- 150
  , 10,  9,  9,  9, 10,  9, 10, 10, 10,  8  -- 160
  ,  9,  9,  9,  9,  9,  9, 10,  9, 10,  9  -- 170
  , 10,  9, 10, 10, 10,  9, 10, 10, 10,  9  -- 180
  , 10, 10, 10,  9, 10, 10, 10, 10, 10, 10  -- 190
  , 11,  8,  9,  9,  9,  9, 10,  9, 10,  9  -- 200
  ]
-}

{-
1: 1
2: 1+1=2
3: 1+1=2, 1+2=3
4: 1+1=2, 2+2=4
5: 1+1=2, 2+2=4, 1+4=5
5: 1+1=2, 1+2=3, 2+3=5
6: 1+1=2, 2+2=4, 2+4=6
6: 1+1=2, 1+2=3, 3+3=6
7: 1+1=2, 2+2=4, 2+4=6, 1+6=7
-}
