module Euler246 where
import Primes
import Permutation
import EulerLib

{-
Problem 246
Tangents to an ellipse

22 May 2009

A definition for an ellipse is:

Given a circle c with centre M and radius r and a point G such that
d(G,M)<r, the locus of the points that are equidistant from c and G
form an ellipse.  The construction of the points of the ellipse is
shown below.

Given are the points M(-2000,1500) and G(8000,1500).  Given is also
the circle c with centre M and radius 15000.  The locus of the points
that are equidistant from G and c form an ellipse e.  From a point P
outside e the two tangents t_(1) and t_(2) to the ellipse are drawn.
Let the points where t_(1) and t_(2) touch the ellipse be R and S.

For how many lattice points P is angle RPS greater than 45 degrees?

-}


{-

The ellipse is centered at (3000, 1500).

We will do the rest of the problem translated so that the
ellipse is centered at the origin (0, 0).

M = (-5000, 0)
G = (+5000, 0)

Ellipse has semimajor axis a = 15000/2 = 7500.
Ellipse satisfies a^2 = b^2 + 5000^2.
Ellipse has semiminor axis b = sqrt 31250000 = 2500 sqrt 5.

For (x, y) inside the ellipse, we have:

x^2 / 56250000 + y^2 / 31250000 < 1
x^2 / 5625 + y^2 / 3125 < 10000
x^2 / 9 + y^2 / 5 < 6250000
x^2 / 9 + y^2 / 5 < 6250000
5 * x^2 + 9 * y^2 < 281250000

-}

type Z = Integer

-- Area of the ellipse:
-- Start at point (7500, 1)  (outside)
-- if inside, then add twice row size to area, move up.
-- if x = 0, then done.
-- if outside, then move left.

-- count points on boundary as inside.
ellipse_area :: Z
ellipse_area = area 7500 1 (2*7500+1)
  where
    area x y t
      | r <= 281250000 = area x (y+1) (t + 4*x+2)
      | x == 0         = t
      | otherwise      = area (x-1) y t
      where r = 5*x^2 + 9*y^2

-- 131715129 lattice points are inside (or on) the ellipse.
-- (only (+/-7500, 0) are on the ellipse)


{-

For a unit circle, and a point P outside the circle,
the angle between the tangent lines is given by:
  ___C
 /  /\`--.
|  A--|---`B
 \___/

Triangle ABC:
  angle C = 90 degrees
  AC = 1
  AB = r
  What is angle ABC?
  sin(ABC) = AC/AB = 1/r
  sin(ABC) = 1/r
  cos(BAC) = 1/r
  ABC = asin(1/r)
-}




{-
B = (r, 0)
C = (cos(BAC), sin(BAC))
C = (1/r, sqrt(1 - 1/r^2))

B = (r cos t, r sin t) = (x, y)
C = (1/r cos t - sqrt(1-1/r^2) sin t, 1/r sin t + sqrt(1-1/r^2) cos t)
C = (x/r^2 - sqrt(1-1/r^2) y/r, y/r^2 + sqrt(1-1/r^2) x/r)
C = (x - r sqrt(1-1/r^2) y, y + r sqrt(1-1/r^2) x) / r^2
C = (x - sqrt(r^2 - 1) y, y + sqrt(r^2 - 1) x) / r^2


(x,y) rotated t -> (x cos t - y sin t, x sin t + y cos t)

Let e = ellipse centered at (0,0),
with semimajor axis a, semiminor axis b:

x^2/a^2 + y^2/b^2 = 1

(x,y)
scale to circle
(h,k) = (x/a, y/b)
r2 = h^2 + k^2
t1 = (h - sqrt(r2 - 1) k, k + sqrt(r2 - 1) h) / r2
t2 = (h + sqrt(r2 - 1) k, k - sqrt(r2 - 1) h) / r2
d1 = (h,k) - t1
d2 = (h,k) - t2
scale to ellipse
d1' = (d1x*a, d1y*b)
d2' = (d2x*a, d2y*b)


(x,y)
scale to circle
(h,k) = (x/a, y/b)
r2 = h^2 + k^2
t1 = (h - sqrt(r2 - 1) k, k + sqrt(r2 - 1) h) / r2
t2 = (h + sqrt(r2 - 1) k, k - sqrt(r2 - 1) h) / r2
scale to ellipse
t1' = (t1x*a, t1y*b)
t2' = (t2x*a, t2y*b)

t1 = (x - sqrt(r2 - 1) y a/b, y + sqrt(r2 - 1) x b/a) / r2
t2 = (x + sqrt(r2 - 1) y a/b, y - sqrt(r2 - 1) x b/a) / r2

a = 56250000
b = 31250000
a/b = 9/5
b/a = 5/9

-}

type R = Double

tangent_points :: (R, R) -> ((R, R), (R, R))
tangent_points (x, y) = (t1, t2)
  where
    t1 = ((x - sqrt(a2/b2*(r2-1)) * y)/r2, (y + sqrt(b2/a2*(r2-1)) * x)/r2)
    t2 = ((x + sqrt(a2/b2*(r2-1)) * y)/r2, (y - sqrt(b2/a2*(r2-1)) * x)/r2)
    r2 = x^2/a2 + y^2/b2
    a2 = 56250000
    b2 = 31250000

check_tangent_points (x, y) = (check t1, check t2)
  where
    (t1, t2) = tangent_points (x, y)
    check (h, k) = h^2/a2 + k^2/b2
    a2 = 56250000
    b2 = 31250000

tangent_angle :: (R, R) -> R
tangent_angle (x, y) = acos cosine * 180 / pi
  where
    ((a,b),(c,d)) = tangent_points (x, y)
    ((i,j),(k,l)) = ((x-a, y-b), (x-c, y-d))
    dot = i*k + j*l
    r1 = sqrt (i^2 + j^2)
    r2 = sqrt (k^2 + l^2)
    cosine = dot / r1 / r2

inside :: Z -> Z -> Bool
inside x y = tangent_angle (fromIntegral x, fromIntegral y) > 45.0

outside :: Z -> Z -> Bool
outside x y = tangent_angle (fromIntegral x, fromIntegral y) <= 45.0

other_area :: Z
other_area = area 15439 1 (2*15439+1)
  where
    area x y t
      | inside x y = area x (y+1) (t + 4*x+2)
      | x == 0     = t
      | otherwise  = area (x-1) y t

{-

Vertically:
(0, 18949) (0, 18950)

Horizontally:
(15439, 0) (15440, 0)

-}

main :: IO String
main = return $ show $ other_area - ellipse_area

answer :: String
answer = "810834388"
