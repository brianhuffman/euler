module Euler184 where
import List
import Ratio

{-
Problem 184
Triangles containing the origin.

29 February 2008

Consider the set I(r) of points (x,y) with integer co-ordinates in the interior
of the circle with radius r, centered at the origin, i.e. x^2 + y^2 <= r^2.

For a radius of 2, I(2) contains the nine points (0,0), (1,0), (1,1), (0,1),
(-1,1), (-1,0), (-1,-1), (0,-1) and (1,-1). There are eight triangles having
all three vertices in I(2) which contain the origin in the interior. Two of
them are shown below, the others are obtained from these by rotation.

*---*   ._* .
.\./.   *._\.
. * .   . .`*

For a radius of 3, there are 360 triangles containing the origin in the
interior and having all vertices in I(3) and for I(5) the number is 10600.

How many triangles are there containing the origin in the interior and having
all three vertices in I(105)?
-}

{-
Whether or not triangle ABC contains the origin O is completely determined
by the angles AOB, BOC, and COA. The triangle contains the origin iff all of
the angles are less than 180 degrees.

If triangle ABC does not contain the origin, then exactly one of the angles
AOB, BOC, or COA will be greater than 180 degrees. Without loss of generality
we will assume that angle COA > 180.

Let a, b, and c represent the angles from the x-axis to rays OA, OB, and OC,
respectively. For a triangle not containing the origin, we have a <= b <= c,
and c <= a + 180.

We can fully categorize all of the triangles that do not contain the origin:
1) a = b = c (collinear)
2) a = b < c
3) a < b < c < a + 180
4) a < b < c = a + 180

-}

type Point = (Integer, Integer)

-- greatest m such that m^2 * (x^2 + y^2) < r^2
multiplicity :: Integer -> Point -> Integer
multiplicity r a =
  List.genericLength $ takeWhile (< r^2) [ m^2 * dist2 a | m <- [1 ..] ]
-- TODO: speed up this function

mediant :: Point -> Point -> Point
mediant (x0, y0) (x1, y1) = (x0+x1, y0+y1)

dist2 :: Point -> Integer
dist2 (x, y) = x^2 + y^2
 
-- ordered list of slope multiplicities for one quadrant of I(r)
multiplicities :: Integer -> [Integer]
multiplicities r = (r-1) : f (1,0) (0,1)
  where
    f a c
      | dist2 b < r^2 = f a b ++ multiplicity r b : f b c
      | otherwise     = []
      where b = mediant a c

{-
multiplicities r =
  map List.genericLength $ group $ sort $
  [ (y%x) | x <- [1 .. r], y <- [0 .. r], x^2 + y^2 < r^2 ]
-}
-- for r=105, there are 5250 different angles per quadrant

choose2 :: Integer -> Integer
choose2 n = n * (n-1) `div` 2

choose3 :: Integer -> Integer
choose3 n = n * (n-1) * (n-2) `div` 6

prob184 r = total - 4 * sum [ f m | m <- ms ]
  where
    ms = multiplicities r
    k = 2 * sum ms
    total = choose3 (2*k)
    f 1 = choose2 (k - 1) +     -- type 3
          (k - 1)               -- type 4
    f m = choose3 m +           -- type 1
          choose2 m * k +       -- type 2
          m * choose2 (k - m) + -- type 3
          m^2 * (k - m)         -- type 4

main :: IO String
main = return $ show $ prob184 105
-- 1725323624056
