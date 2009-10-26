module Euler253 where
import Permutation
import Data.List (insert, sort, group)
import EulerLib
import Data.Ratio
import Data.Array

{-
Problem 253
Tidying Up

28 August 2009

A small child has a “number caterpillar” consisting of forty jigsaw
pieces, each with one number on it, which, when connected together in
a line, reveal the numbers 1 to 40 in order.

Every night, the child's father has to pick up the pieces of the
caterpillar that have been scattered across the play room. He picks up
the pieces at random and places them in the correct order.

As the caterpillar is built up in this way, it forms distinct segments
that gradually merge together.

The number of segments starts at zero (no pieces placed), generally
increases up to about eleven or twelve, then tends to drop again
before finishing at a single segment (all pieces placed).

For example:

Piece Placed 	Segments So Far
12              1
4               2
29              3
6               4
34              5
5               4
35              4
…               …

Let M be the maximum number of segments encountered during a random
tidy-up of the caterpillar.

For a caterpillar of ten pieces, the number of possibilities for each M is

M       Possibilities
1       512      
2       250912      
3       1815264      
4       1418112      
5       144000      

so the most likely value of M is 3 and the average value is
^(385643)/_(113400) = 3.400732, rounded to six decimal places.

The most likely value of M for a forty-piece caterpillar is 11; but
what is the average value of M?

Give your answer rounded to six decimal places.

-}

----------------------------------------------------------------------
-- Brute force for small n

type P = Char

shuffles :: Int -> [[P]]
shuffles n = permutations $ take n $ ['a' ..]

-- precondition: sorted input
pieces :: [P] -> Int
pieces [] = 0
pieces (_ : []) = 1
pieces (x : xs@(y : ys))
  | y == succ x = pieces xs
  | otherwise = 1 + pieces xs

-- max_pieces :: [P] -> Int
max_pieces =
  -- maximum .
  map pieces . scanr insert []

-- hist :: Int -> [Int]
hist = map (\x -> (length x, head x)) . group . sort . map max_pieces . shuffles

----------------------------------------------------------------------

table :: Int -> Int -> Array (Int, Int) Integer
table nmax m = a
  where
    a = funArray ((1,0), (nmax, m+1)) f
    f (1, 1) = 1
    f (1, s) = 0
    f (n, s)
      | s > m = 0
      | s < 1 = 0
      | otherwise =
          fromIntegral s *
          ( a!(n-1, s-1) +
            a!(n-1, s)*2 +
            a!(n-1, s+1) )

shuffles_upto :: Int -> Int -> Integer
shuffles_upto n m = table n m ! (n, 1)

shuffle_hist :: Int -> Histogram
shuffle_hist n = zipWith (-) xs (0 : xs)
  where
    mmax = (n + 1) `div` 2
    xs = [ shuffles_upto n m | m <- [1 .. mmax] ]

shuffle_average :: Int -> Rational
shuffle_average n = avgHist (shuffle_hist n)


{-

*Euler253> zip [1..] $ hist 1
[(1,1)]
*Euler253> zip [1..] $ hist 2
[(1,2)]
*Euler253> zip [1..] $ hist 3
[(1,4),(2,2)]
*Euler253> zip [1..] $ hist 4
[(1,8),(2,16)]
*Euler253> zip [1..] $ hist 5
[(1,16),(2,92),(3,12)]
*Euler253> zip [1..] $ hist 6
[(1,32),(2,472),(3,216)]
*Euler253> zip [1..] $ hist 7
[(1,64),(2,2312),(3,2520),(4,144)]
*Euler253> zip [1..] $ hist 8
[(1,128),(2,11104),(3,24480),(4,4608)]
*Euler253> zip [1..] $ hist 9
[(1,256),(2,52880),(3,216432),(4,90432),(5,2880)]


Imagine the process happening in reverse: The state can be represented
as a list of the remaining segments' sizes.

1234567890  [10]
123 567890  [3,6]
123 5 7890  [1,3,4]
 23 5 7890  [1,2,4]
 23 5 78 0  [1,1,2,2]
 23   78 0  [1,2,2]
  3   78 0  [1,1,2]
  3   78    [1,2]
  3    8    [1,1]
  3         [1]
            []

For any two states with the same size list, the future distribution of
possible max-segments is the same.  We just need to make a DP table of
max-segment distributions for each possible size list.

How to represent the size list?

How many size lists are there?

Size 1: [1]
Size 2: [2]
Size 3: [1,1] [3]
Size 4: [1,2] [4]
Size 5: [1,1,1] [1,3] [2,2] [5]
Size 6: [1,1,2] [1,4] [2,3] [6]
Size 7: [1,1,1,1] [1,1,3] [1,2,2] [1,5] [2,4] [3,3] [7]
Size 8: [1,1,1,2] [1,1,4] [1,2,3] [2,2,2] [1,6] [2,5] [3,4] [8]
Size 9: [1,1,1,1,1] [1,1,1,3] [1,1,2,2] [1,1,5] [1,2,4] [1,3,3]
        [2,2,3] [1,7] [2,6] [3,5] [4,4] [9]


1, 1, 2, 2, 4, 4, 7, 8, 12 ...

A002865

Number of partitions of n that do not contain 1 as a part.

1, 0, 1, 1, 2, 2, 4, 4, 7, 8, 12, 14, 21, 24, 34, 41, 55, 66, 88, 105,
137, 165, 210, 253, 320, 383, 478, 574, 708, 847, 1039, 1238, 1507,
1794, 2167, 2573, 3094, 3660, 4378, 5170, 6153, 7245, 8591, 10087,
11914, 13959, 16424, 19196, 22519, 26252, 30701

Up to size 10, there are 55 different size lists.
Up to size 20, there are 791 different size lists.
Up to size 30, there are 6841 different size lists.
Up to size 40, there are 44582 different size lists.

-}

type Histogram = [Integer]
-- first is number for max 0, second for max 1, etc.

type SizeList = [Int]
-- in sorted order

data MemoTree = Node Histogram [MemoTree]

look :: MemoTree -> SizeList -> Histogram
look (Node r ts) [] = r
look (Node r ts) (x : xs) = look (ts !! x) xs

nextSizeLists :: SizeList -> [SizeList]
nextSizeLists sizes =
  [ insert' x (insert' y ns) |
    (n, ns) <- remove1 sizes,
    x <- [0 .. n-1],
    let y = n-1-x
  ]
  where insert' x xs = if x == 0 then xs else insert x xs

addHist :: Histogram -> Histogram -> Histogram
addHist (x : xs) (y : ys) = x + y : addHist xs ys
addHist xs [] = xs
addHist [] ys = ys

addHists :: [Histogram] -> Histogram
addHists = foldl addHist []

bumpHist :: Int -> Histogram -> Histogram
bumpHist = go 0
  where
    go a n [] = go a n [0]
    go a 1 (x : xs) = x+a : xs
    go a n (x : xs) = 0 : go (x+a) (n-1) xs

memo :: MemoTree
memo = Node [1] [ go [x] | x <- [0 ..] ]
  where
    go xs = Node h' [ go (x : xs) | x <- [0 ..] ]
      where
        l = reverse xs
        ls = nextSizeLists l
        hs = map (look memo) ls
        h' = bumpHist (length l) (addHists hs)

avgHist :: Histogram -> Rational
avgHist h = sum (zipWith (*) [1 ..] h) % sum h

main :: IO String
--main = return $ showFloat 6 $ avgHist $ look memo [40]
main = return $ showFloat 6 $ shuffle_average 40

answer :: String
answer = "11.492847"
