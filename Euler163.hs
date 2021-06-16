module Euler163 where
import EulerLib
import Data.List

{-
Problem 163
Cross-hatched triangles

13 October 2007

Consider an equilateral triangle in which straight lines are drawn from each
vertex to the middle of the opposite side, such as in the size 1 triangle in
the sketch below.

      *
     /|\
    / | \
   *-.|.-*
  /  .*.  \
 /.-' | `-.\
*-----*-----*
    size 1


            *
           /|\
          / | \
         *-.|.-*
        /  .*.  \
       /.-' | `-.\
      *-----*-----*
     /|\`-. | .-'/|\
    / | \  .*.  / | \
   *-.|.-*' | `*-.|.-*
  /  .*.  \ | /  .*.  \
 /.-' | `-.\|/.-' | `-.\
*-----*-----*-----*-----*
          size 2

Sixteen triangles of either different shape or size or orientation or location
can now be observed in that triangle. Using size 1 triangles as building blocks,
larger triangles can be formed, such as the size 2 triangle in the above sketch.
One-hundred and four triangles of either different shape or size or orientation
or location can now be observed in that size 2 triangle.

It can be observed that the size 2 triangle contains 4 size 1 triangle building
blocks. A size 3 triangle would contain 9 size 1 triangle building blocks and a
size n triangle would thus contain n2 size 1 triangle building blocks.

If we denote T(n) as the number of triangles present in a triangle of size n,
then

T(1) = 16
T(2) = 104

Find T(36).
-}

{-
A: 60/60/60 triangles come in 8 orientations
  A1: pointing up (area 6)
  A2: pointing down (area 6)
  A3: 6 equivalent pointing left, right (area 2)
  A4: 6 equivalent pointing left, right (area 8)
  A5: 2 equivalent pointing left, right (area 18)

B: 30/60/90 triangles come in 24 orientations
   B1: 6 equivalent orientations in size 1 triangle (area 1).
   B2: 6 equivalent are inverted versions of first 6 (area 1).
   B3: 6 equivalent in size 1 triangle (area 3).
   B4: 6 equivalent inverted (area 3).

C: 30/30/120 triangles come in 12 orientations
   C1: 3 equivalent orientations in size 1 triangle (area 2).
   C2: 3 equivalent inverted versions (area 2).
   C3: 6 equivalent orientations (area 6).

scale factor n:
A1: (1) starting at n
A2: (1) starting at 2*n
A3: (6) starting at 3*n-1
A4: (6) starting at 3*n
A5: (2) starting at 3*n
B1: (6) starting at [1,2,2;3,4,4;...
B2: (6) starting at [2,2,3,4,5,5;...
B3: (6) starting at n
B4: (6) starting at [2,3;5,6;...
C1: (3) starting at n
C2: (3) starting at [2,3,4;...
C3: (6) starting at 2*n

T(1) = 16
T(2) = 104
T(3) = 303
T(4) = 653
T(5) = 1196
T(6) = 1978
T(7) = 3032
T(8) = 4410
T(9) = 6148
T(10) = 8292
T(11) = 10875
T(12) = 13995
T(13) = 17556

[16,104,303,653,1196,1978,3032,4410,6148,8292,10875,13955,17556]

new triangles
N(1) = 16
N(2) = 56
N(3) = 39
N(4) = 40
N(5) = 42
N(6) = 46
N(7) = 33
N(8) = 52
N(9) = 36
N(10) = 46
N(11) = 33
N(12) = 58
N(13) = 24

[16,56,39,40,42,46,33,52,36,46,33,58,24]

[16,104,303,653,1196,1978,3032,4410,6148,8292,10875,13995]
[88,199,350,543,782,1054,1378,1738,2144,2583,3120]
[111,151,193,239,272,324,360,406,439,537]
[40,42,46,33,52,36,46,33,98]

T(1) = 16
area 1: 6 (B1.1)
area 2: 3 (C1.1)
area 3: 6 (B3.1)
area 6: 1 (A1.1)

T(2) = 104
48 triangles from previous sizes
area 1: 6 (B2.1)
area 2: 3 (C2.1) + 6 (A3)
area 3: 6 (B4.1)
area 4: 6 (B1.2) + 6 (B2.2)
area 6: 1 (A2.1) + 6 (C3.1)
area 8: 3 (C1.2)
area 9: 6 (B1.3)
area 12: 6 (B3.2)
area 24: 1 (A1.2)

T(3) =
240 triangles from previous sizes
area 8: 6 (A4)
area 8: 3 (C2.2)
area 9: 6 (B2.3)
area 12: 6 (B4.2)
area 16: 6 (B1.4)
area 18: 2 (A5.1)
area 18: 3 (C1.3)
area 27: 6 (B3.3)
area 54: 1 (A1.3)

area 1: 6 (B2)
area 2: 3 (C2) + 6 (A3)
area 3: 6 (B4)
area 4: 6 (B1) + 6 (B2)
area 6: 1 (A2) + 6 (C3)
area 8: 3 (C1)
area 9: 6 (B1)
area 12: 6 (B3)
area 24: 1 (A1)

area 1: 54
area 2: 27 (C) + 18 (A)
area 3: 

-}

type Point = (Int, Int)
-- coordinates of points on lattice of size n
lattice :: Int -> [Point]
lattice n =
  [ (x,y) |
    mx <- [0 .. n],
    my <- [0 .. n],
    (dx,dy) <- [(0,0),(1,3),(2,0),(2,2),(3,3),(4,4)],
    let x = 4*mx + 2*my + dx,
    let y = 6*my + dy,
    3*x + y <= 12*n ]

-- Brute force solution
all_triangles :: Int -> [(Point, Point, Point)]
all_triangles n =
  [ (p1,p2,p3) |
    (p1:ps1) <- tails points,
    (p2:ps2) <- tails ps1,
    any (inline p1 p2) angles,
    p3 <- ps2,
    any (inline p1 p3) angles,
    any (inline p2 p3) angles,
    not (collinear p1 p2 p3) ]
  where
    points = lattice n
    angles = [(0,2),(3,3),(3,1),(6,0),(3,-1),(3,-3)]
    inline (x1,y1) (x2,y2) (ax,ay) =
      let r1 = ax*x1 + ay*y1
          r2 = ax*x2 + ay*y2
      in r1 == r2 && divides 12 r1
    collinear (x0,y0) (x1,y1) (x2,y2) =
      let dx1 = x1 - x0
          dy1 = y1 - y0
          dx2 = x2 - x0
          dy2 = y2 - y0
      in dx1*dy2 == dx2*dy1

-- how many triangles first appear at size n?
new_triangles :: [Int]
new_triangles = foldl1 (zipWith (+)) $
  [ cycle [1]                -- A1  (fits 1:1)  (area 6)
  , cycle [0,1]              -- A2  (fits 1:2)  (area 6)
  , cycle [0,0,2]            -- A?  (fits 1:3)  (area 18)
  , [0,6,6] ++ cycle [2,8,6] -- all other equilateral
  , cycle [6,12]        -- B1  (fits 3:2)
  , cycle [0,12,6,6,12] -- B2  (fits 6:5)
  , cycle [6]           -- B3  (fits 1:1)
  , cycle [0,6,6]       -- B4  (fits 2:3)
  , cycle [3]           -- C1  (fits 1:1)
  , cycle [0,3,3,3]     -- C2  (fits 3:4)
  , cycle [0,6]         -- C3  (fits 1:2)
  ]
-- [16,56,39,40,42,46,33,52,36,46,33,58,24]

total_triangles :: Int -> Int
total_triangles n =
  sum $
  map (\(m,n) -> triangle m * n) $
  zip [1 ..] $
  reverse $
  take n new_triangles

main :: IO String
main = return $ show $ total_triangles 36
-- 343047
