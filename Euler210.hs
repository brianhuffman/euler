module Euler210 where
import Data.Int ( Int64 )

{-
Consider the set S(r) of points (x,y) with integer coordinates satisfying
|x| + |y| <= r.
Let O be the point (0,0) and C the point (r/4,r/4).
Let N(r) be the number of points B in S(r), so that the triangle OBC has
an obtuse angle, i.e. the largest angle α satisfies 90° < α < 180°.
So, for example, N(4)=24 and N(8)=100.

What is N(1,000,000,000)? 
-}


{-

        *
      = * *
    = - = * /
  * * = = C * *
* * * * O = = * *
  * * / * = - =
    / * * * =
      * * *
        *


                *
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

1, 2, ...
8, 32, ...
4, 16, ...

N(x) ~ (3/2 + pi/32)(x^2)

+-C
|/ 
O

+---C  (m=4)
|**/
|*/
|/
O
triangle (m-2) = 
-}

type Z = Int64

near_answer :: (Floating a) => a -> a
near_answer r = (3/2 + pi/32) * r^2
-- near_answer (10^9) = 1.5981747704246812e18

region_O :: Z -> Z
region_O r = 8 * n^2
  where n = r `div` 4

region_C :: Z -> Z
region_C r = 4 * n^2
  where n = r `div` 4

-- precondition: 8 divides r
region_B :: Z -> Z
region_B r = 2 * f 1 0 0 + a
  where
    n = r `div` 4
    m = r `div` 8
    d = 2*m^2
    a = (n-2)*(n-1)`div`2
    f y x' t
      | y > m = t
      | (y-m)^2 + (x'+m)^2 < d = f y (x'+1) t
      | otherwise = f (y+1) x' $! t'
          where t' = (if y<m then 2*x' else x') + t

prob210 :: Z -> Z
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
main = return $ show $ prob210 (10^8)

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

