module Euler252 where
import Data.Array
import Data.List (sortBy, tails, foldl')

import EulerLib

{-
Problem 252
Convex Holes

27 June 2009

Given a set of points on a plane, we define a convex hole to be a
convex polygon having as vertices any of the given points and not
containing any of the given points in its interior (in addition to the
vertices, other given points may lie on the perimeter of the polygon).

As an example, the image below shows a set of twenty points and a few
such convex holes. The convex hole shown as a red heptagon has an area
equal to 1049694.5 square units, which is the highest possible area
for a convex hole on the given set of points.

For our example, we used the first 20 points (T_(2k−1), T_(2k)), for
k = 1,2,…,20, produced with the pseudo-random number generator:

S_(0) 	= 	290797
S_(n+1) = 	S_(n)^(2) mod 50515093
T_(n) 	= 	( S_(n) mod 2000 ) − 1000

i.e. (527, 144), (−488, 732), (−454, −947), …

What is the maximum area for a convex hole on the set containing the
first 500 points in the pseudo-random sequence?  Specify your answer
including one digit after the decimal point.

-}

type Z = Int
type Pt = (Z, Z)

prng :: [Z]
prng = map t (tail xs)
  where
    xs :: [Integer]
    xs = iterate next 290797
    next x = (x^2) `mod` 50515093
    t x = fromIntegral (x `mod` 2000) -- 1000

points :: [Pt]
points = pairs prng
  where pairs (x : y : zs) = (x, y) : pairs zs

----------------------

-- returns twice the area
-- positive if oriented counter-clockwise.
-- negative if oriented clockwise.
area2 :: Pt -> Pt -> Pt -> Z
area2 (x0,y0) (x1,y1) (x2,y2) = dx1*dy2 - dx2*dy1
  where
    (dx1, dy1) = (x1-x0, y1-y0)
    (dx2, dy2) = (x2-x0, y2-y0)


-- precondition:
-- input list is sorted right-to-left, as viewed from point a.
-- postconditions:
-- output list is subsequence of input.
-- output list is sorted right-to-left, as viewed from point a OR b.
-- output triangles abc contain no other points from input.
foo :: Pt -> [Pt] -> [Pt]
foo b [] = []
foo b (c : cs) = c : baz c cs
  where
    baz c [] = []
    baz c (c' : cs)
      | 0 <= area2 b c c' = c' : baz c' cs
      | otherwise         = baz c cs

-- preconditions:
-- all cs are to the left of line ab (i.e. abc has positive area).
-- cs are sorted from right-to-left, as viewed from a OR b.
-- (i.e. angle bac is smallest when c = head cs)
-- (also, all triangles abc should contain no other points)
max_area :: Pt -> Pt -> [Pt] -> (Z, [Pt])
max_area a b cs = foldl' max (0, [])
  [ (area2 a b c + area', c : pts) |
    c : cs2 <- tails cs,
    let cs3 = foo c cs2,
    let cs4 = [ c' | c' <- cs3, area2 b c c' > 0 ],
    let (area', pts) = max_area a c cs4
  ]

-- WLOG assume 'a' precedes 'b'.
biggest_hole :: [Pt] -> (Z, [Pt])
biggest_hole pts = maximum
  [ (area, a : b : pts) |
    a : bs <- tails pts,
    b <- bs,
    let cs0 = [ c | c <- pts, area2 a b c > 0 ],
    let cs1 = sortBy (\x y -> compare 0 (area2 a x y)) cs0,
    let cs = foo b cs1,
    let (area, pts) = max_area a b cs
  ]

prob252 :: [Pt] -> Double
prob252 pts = fromIntegral (fst (biggest_hole pts)) / 2

main :: IO String
main = return $ show $ prob252 (take 500 points)

answer :: String
answer = "104924.0"

----------------------

grid :: Int -> (Z, Array (Z, Z) Int)
grid n = (area, arr)
  where
    pts = take n points
    (area, poly) = biggest_hole pts
    arr = accumArray (+) 0 ((0,0), (99,39))
      [ ((x`div`20, y`div`50), 1) | (x,y) <- pts ++ poly ++ poly ]

showGrid :: Int -> (Z, [String])
showGrid n =
  (area, [ [ " .:*#%" !! (g ! (x,y)) | x <- [0..99] ] | y <- [39,38..0] ])
  where (area, g) = grid n

printGrid :: Int -> IO ()
printGrid n = mapM_ print ls >> print (fromIntegral area / 2)
  where (area, ls) = showGrid n
