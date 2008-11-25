module Euler199 where

--------------------------------------------------------------------------------
-- 199. Iterative Circle Packing
{-
Three circles of equal radius are placed inside a larger circle such that each
pair of circles is tangent to one another and the inner circles do not overlap.
There are four uncovered "gaps" which are to be filled iteratively with more
tangent circles.

At each iteration, a maximally sized circle is placed in each gap, which creates
more gaps for the next iteration. After 3 iterations (pictured), there are 108
gaps and the fraction of the area which is not covered by circles is 0.06790342,
rounded to eight decimal places.

What fraction of the area is not covered by circles after 10 iterations?
Give your answer rounded to eight decimal places using the format x.xxxxxxxx .
-}

{-
Locating a circle tangent to three other circles (Apollonius' problem).

(x-x1)^2 + (y-y1)^2 - (r+-r1)^2 = 0
(x-x2)^2 + (y-y2)^2 - (r+-r2)^2 = 0
(x-x3)^2 + (y-y3)^2 - (r+-r3)^2 = 0

(+) for outside, (-) for inside.

(x^2 + y^2 - r^2) - 2.x.xi - 2.y.yi -+ 2.r.ri + (xi^2 + yi^2 - ri^2) = 0

ax + by + cr = d
  where
    a = 2(x1-x2)
    b = 2(y1-y2)
    c = +/-2(r1-r2)
    d = (x1^2 + y1^2 - r1^2) - (x2^2 + y2^2 - r2^2)

(and similarly for subscripts 2 replaced by 3)


x = (b'd - bd' - b'cr + bc'r) / (ab' - ba')
y = (-a'd + ad' + a'cr - ac'r) / (ab' - ba')


Soddy circles:
2(e1^2 + e2^2 + e3^2 + e4^2) = (e1 + e2 + e3 + e4)^2
where ei = +-ki = +-1/ri

r4(+-) = r1.r2.r3 / [r1.r2 + r1.r3 + r2.r3 +- 2.sqrt(r1.r2.r3.(r1+r2+r3)) ]
-}

curvature :: Double -> Double -> Double -> Double
curvature a b c = a + b + c + 2 * sqrt (a*b + b*c + c*a)

curvature' :: Double -> Double -> Double -> Double
curvature' a b c = a + b + c - 2 * sqrt (a*b + b*c + c*a)

{-
curvature' k0 k0 k0 = -1
k0 + k0 + k0 - 2 * sqrt (k0*k0 + k0*k0 + k0*k0) = -1
3*k0 - 2 * sqrt (3*k0*k0) = -1
3*k0 - 2 * k0 * sqrt 3 = -1
k0 * (3 - 2 * sqrt 3) = -1
k0 = -1 / (3 - 2 * sqrt 3)
-}

k0 :: Double
k0 = 1 / (2 * sqrt 3 - 3)

area k = 1 / k^2

total_area 0 _ _ _ = 0
total_area n a b c =
  area d + total_area (n-1) a b d
         + total_area (n-1) a d c
         + total_area (n-1) d b c
  where d = curvature a b c

-- total_area2 n a c = total_area n a a c
total_area2 0 _ _ = 0
total_area2 n a c =
  area d + total_area2 (n-1) a d
         + 2 * total_area (n-1) a d c
  where d = curvature a a c

-- total_area3 n a = total_area n a a a
total_area3 0 _ = 0
total_area3 n a =
  area d + 3 * total_area2 (n-1) a d
  where d = curvature a a a

remaining_area n =
  1 - 3 * area k0
    - 3 * total_area2 n k0 (-1)
    - total_area3 n k0

prob199 n =
  show (floor x) ++ "." ++ replicate (d - length (show r)) '0' ++ show r
  where
    d = 8
    x = remaining_area n
    r = round (x * 10^d) `mod` (10^d)

main :: IO String
main = return $ prob199 10

------------------------------------------------------------
{-
alternate method of calculation:
Start with (a, b, c, d) that work.
(Assume that d is the largest curvature value.)

Recurse with
(a, b, d, 2a+2b+2d-c)
(a, c, d, 2a+2c+2d-b)
(b, c, d, 2b+2c+2d-a)

-}

total_area' 0 _ _ _ _ = 0
total_area' n a b c d =
  area d + total_area' (n-1) a b d (2*(a+b+d)-c)
         + total_area' (n-1) a c d (2*(a+c+d)-b)
         + total_area' (n-1) b c d (2*(b+c+d)-a)
-- invariant: d = curvature a b c

-- total_area2' n a c d = total_area' n a a c d
-- total_area3' n a d   = total_area' n a a a d

total_area2' 0 _ _ _ = 0
total_area2' n a c d =
  area d + total_area2' (n-1) a d (2*(a+a+d)-c)
         + 2 * total_area' (n-1) a c d (2*(a+c+d)-a)

total_area3' 0 _ _ = 0
total_area3' n a d =
  area d + 3 * total_area2' (n-1) a d (3*a+2*d)

remaining_area' n =
  1 - 3 * area k0
    - 3 * total_area2' n k0 (-1) (curvature k0 k0 (-1)) 
    - total_area3' n k0 (curvature k0 k0 k0)

-------------------------------------------------------------
data Tree3 a = Tip3 | Node3 a (Tree3 a) (Tree3 a) (Tree3 a)
  deriving Show

t3take 0 _ = Tip3
t3take n Tip3 = Tip3
t3take n (Node3 a x y z) =
  Node3 a (t3take (n-1) x)
          (t3take (n-1) y)
          (t3take (n-1) z)

t3list Tip3 = []
t3list (Node3 a x y z) = a : t3list x ++ t3list y ++ t3list z

curvatures a b c =
  Node3 (d,a,b,c)
    (curvatures a b d)
    (curvatures a c d)
    (curvatures b c d)
  where d = curvature a b c
