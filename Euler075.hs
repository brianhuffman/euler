module Euler075 where
import Pythagorean
import Data.Array.Unboxed

------------------------------------------------------------------------------
-- 75. Find the number of different lengths of wire can that can form a right angle triangle in only one way.
{-
It turns out that 12 cm is the smallest length of wire can be bent to form a
right angle triangle in exactly one way, but there are many more examples.

12 cm: (3,4,5)
24 cm: (6,8,10)
30 cm: (5,12,13)
36 cm: (9,12,15)
40 cm: (8,15,17)
48 cm: (12,16,20)

In contrast, some lengths of wire, like 20 cm, cannot be bent to form a right
angle triangle, and other lengths allow more than one solution to be found;
for example, using 120 cm it is possible to form exactly three different right
angle triangles.

120 cm: (30,40,50), (20,48,52), (24,45,51)

Given that L is the length of the wire, for how many values of L <= 2,000,000
can exactly one right angle triangle be formed?
-}

right_angles :: Int -> UArray Int Int
right_angles m = a
  where
    ls = [ a+b+c | (a,b,c) <- primitive_pythagorean_triples m ]
    ls' = [ (l*k, 1) | l <- ls, k <- [1 .. m `div` l] ]
    a = accumArray (+) 0 (1, m) ls'

one_right_angle :: Int -> Int
one_right_angle m = length $ filter (==1) $ elems $ right_angles m

main :: IO String
main = return $ show $ one_right_angle (2*10^6)
-- 214954
