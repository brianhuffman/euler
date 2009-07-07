module Euler247 where
import qualified SortedList as S

{-
Problem 247
Squares under a hyperbola

29 May 2009

Consider the region constrained by 1 ≤ x and 0 ≤ y ≤ 1/x.

Let S_(1) be the largest square that can fit under the curve.  Let
S_(2) be the largest square that fits in the remaining area, and so
on.  Let the index of S_(n) be the pair (left, below) indicating the
number of squares to the left of S_(n) and the number of squares below
S_(n).

66

12 32 75
         50 88



1        2        3       4      5

The diagram shows some such squares labelled by number.  S_(2) has one
square to its left and none below, so the index of S_(2) is (1,0).  It
can be seen that the index of S_(32) is (1,1) as is the index of
S_(50).  50 is the largest n for which the index of S_(n) is (1,1).

What is the largest n for which the index of S_(n) is (3,3)?

-}

type R = Double

square_from :: (R, R) -> R
square_from (x, y) = (sqrt ((x-y)^2 + 4) - (x+y)) / 2

data Dir = U | R
  deriving Show

{-
type Index = [Dir]

goU, goR :: Index -> Index
goU ds = U : ds
goR ds = R : ds

none :: Index
none = []
-}

type Index = (Int, Int)

goU, goR :: Index -> Index
goU (i, j) = (i, j+1)
goR (i, j) = (i+1, j)

none :: Index
none = (0, 0)

squares_from (x,y) i = (r,i) : others
  where
    r = square_from (x, y)
    qs1 = squares_from (x+r,y) (goR i)
    qs2 = squares_from (x,y+r) (goU i)
    others = S.mergeBy (\(r1,_) (r2,_) -> compare r2 r1) qs1 qs2

squares :: [(R, Index)]
squares = squares_from (1,0) none

indices :: [(Int, Index)]
indices = zip [1..] (map snd squares)

square_at :: [Dir] -> R
square_at ds = square_from (foldl next (1,0) ds)
  where
    next (x,y) R = (x + square_from (x,y), y)
    next (x,y) U = (x, y + square_from (x,y))

paths :: Int -> Int -> [[Dir]]
paths i 0 = [replicate i R]
paths 0 j = [replicate j U]
paths i j =
  map (R:) (paths (i-1) j) ++ map (U:) (paths i (j-1))

count_squares :: R -> Int
count_squares cutoff = count_from (1,0)
  where
    count_from (x,y)
      | r < cutoff = 0
      | otherwise = 1 + count_from (x+r,y) + count_from (x,y+r)
      where r = square_from (x,y)

count_squares' :: R -> Int
count_squares' cutoff = count_from [(1,0)] 0
  where
    count_from [] t = t
    count_from ((x,y) : points) t
      | r < cutoff = count_from points t
      | otherwise = count_from ((x+r,y) : (x,y+r) : points) (t+1)
      where r = square_from (x,y)

test1 = count_squares (square_at [R,U])

test3 = count_squares (square_at [R,R,R,U,U,U])
test3' = count_squares' (square_at [R,R,R,U,U,U])

{-

(1,1)
RU
UR

(2,2)
AABB
ABAB
ABBA
BAAB
BABA
BBAA



y+a = 1/(x+a)
(x+a)(y+a) = 1
xy + (x+y)a + a^2 = 1
a^2 + (x+y)a + (xy-1) = 0

-B +- sqrt(B^2 - 4AC)
---------------------
        2A

A = 1
B = (x+y)
C = xy-1

[ -(x+y) +- sqrt((x+y)^2 - 4(xy-1)) ] / 2

[ -(x+y) +- sqrt(x^2 + y^2 + 2xy - 4xy + 4) ] / 2

[ -(x+y) +- sqrt(x^2 + y^2 - 2xy + 4) ] / 2

[ -(x+y) +- sqrt((x-y)^2 + 4) ] / 2
-}


main :: IO String
main = return $ show $ test3'

answer :: String
answer = "782252"
