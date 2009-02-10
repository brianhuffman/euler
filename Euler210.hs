module Euler210 where
import Data.Int ( Int64 )

{-
Problem 210
26 September 2008

Consider the set S(r) of points (x,y) with integer coordinates
satisfying |x| + |y| ≤ r.

Let O be the point (0,0) and C the point (r/4,r/4).

Let N(r) be the number of points B in S(r), so that the triangle OBC
has an obtuse angle, i.e. the largest angle α satisfies 90°<α<180°.

So, for example, N(4)=24 and N(8)=100.

What is N(1,000,000,000)?

-}


{-

If point B lies in the left or lower quadrants, then angle O will be
obtuse. We will call these areas "region O".

If point B lies in the outer half of the upper or right quadrants,
then angle C will be obtuse. We will call these areas "region C".

If point B lies in the interior of the circle with diameter O-C, then
angle B will be obtuse. We will call these areas "region B".

-----------------------------------------
r = 4:  *
      \ * *
    \ - \ * /
  * * \ = C * *
* * * * O = \ * *
  * * / * \ - \
    / * * * \
      * * *
        *
Region O (8), region C (4), region B (0).
-----------------------------------------
r = 8:          *
              * * *
            \ * * * *
          . . \ * * * *
        \ . . . \ * * * /
      * * \ . . . \ * / * *
    * * * * \ . = * C * * * *
  * * * * * * \ * / * \ * * * *
* * * * * * * * O * = . \ * * * *
  * * * * * * / * \ . . . \ * *
    * * * * / * * * \ . . . \
      * * / * * * * * \ . .
        / * * * * * * * \
          * * * * * * *
            * * * * *
              * * *
                *
Region O (32), region C (16), region B (2).
-----------------------------------------

N(x) ~ (3/2 + pi/32)(x^2)
-}

square :: Int -> Int64
square x = (fromIntegral x)^2

near_answer :: (Floating a) => a -> a
near_answer r = (3/2 + pi/32) * r^2
-- near_answer (10^9) = 1.5981747704246812e18

region_O :: Int -> Int64
region_O r = 8 * square n
  where n = r `div` 4

region_C :: Int -> Int64
region_C r = 4 * square n
  where n = r `div` 4

-- precondition: 8 divides r
region_B :: Int -> Int64
region_B r = a + 2 * f m m 0 0
  where
    -- point C is at (n, n)
    n = r `div` 4
    -- center of circle is at (m, m)
    m = r `div` 8
    -- radius squared of circle
    d = 2 * square m
    -- points in interior of 90-45-45 triangle with hypotenuse OC
    a = fromIntegral (n-2) * fromIntegral (n-1) `div` 2
    -- points in interior of circle with radius-squared = d,
    -- with x-coordinate greater than x.
    -- invariant: r2 = x^2 + y^2 - 2*m^2
    -- invariant: 0 <= r2 + 2*y + 1 (i.e. (x,y+1) not inside circle)
    f x y r2 t
      | y < 0 = t
      | r2 >= 0 = f x (y-1) (r2-2*y+1) t
      | otherwise = f (x+1) (y-dy) (r2+2*x+1-2*y*dy+dy^2) $! t'
          where dy = x `div` (y+1)
                t' = t + fromIntegral (2*y+1)

{-
0 <= r2 + 2*y + 1
dy = x `div` (y+1)
 -->
dy*(y+1) <= x
 <-->
0 <= x - dy*y - dy
 <-->
0 <= 2*x - 2*dy*y - 2*dy
 -->
0 <= (r2 + 2*y + 1) + (2*x - 2*dy*y - 2*dy)
 -->
0 <= (r2 + 2*y + 1) + (2*x - 2*dy*y - 2*dy) + (dy^2 + 1)
 <-->
0 <= (r2 + 2*x - 2*dy*y + dy^2 + 1) + 2*(y-dy) + 1
-}


prob210 :: Int -> Int64
prob210 r =
  2 * region_O r +
  2 * region_C r +
  2 * region_B r

{-
N(10^3) = 1597880
N(10^4) = 159814790
N(10^5) = 15981722482
N(10^6) = 1598174519142
N(10^7) = 159817474536486
N(10^8) = 15981747679237090
N(10^9) = 1598174770174689458
N(10^9) ~ 1.5981747704246812e18
-}

main :: IO String
main = return $ show $ prob210 (10^9)

answer :: String
answer = "1598174770174689458"

---------------------------------------------
-- slow version used as a check

obtuse (a,b) (c,d) (x,y) = dot < 0 && cross /= 0
  where
    (x1,y1) = (a-x, b-y)
    (x2,y2) = (c-x, d-y)
    dot = x1 * x2 + y1 * y2
    cross = x1 * y2 - x2 * y1

obtuse' a b c =
  obtuse a b c ||
  obtuse b c a ||
  obtuse c a b

points r =
  [ (x, y) |
    x <- [-r .. r],
    let s = r - abs x,
    y <- [-s .. s] ]

prob210a r = [ b | b <- points r, obtuse' o b c ]
  where
    o = (0, 0)
    s = r `div` 4
    c = (s, s)

prob210b = length . prob210a

{-
                *
               ***
              *****
             *******
            *********
           ***********
          *************
         ***************
        -***************-
       **-*************---
      ****-***********-----
     ******-*********-------
    ********-*******---------
   **********-*****---------**
  ************-***---------****
 **************-*---------******
****************O***-----********
 **************-*-***---********
  ************--**-**--********
   **********---***-*-********
    ********-----***C********
     ******---------*-******
      ****---------***-****
       **---------*****-**
        ---------*******-
         -------********
          -----********
           ---********
            -********
             *******
              *****
               ***
                *

-***C
***-*
**-**
*-***
O***-

-}

