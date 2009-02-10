module Euler177 where
import EulerLib
import Data.Array.Unboxed
import Maybe

{-
Problem 177
Integer angled Quadrilaterals

11 January 2008

Let ABCD be a convex quadrilateral, with diagonals AC and BD. At each
vertex the diagonal makes an angle with each of the two sides,
creating eight corner angles.

 D------C
 |\    /|
 | \  / |
 |  \/  |
 |  /\  |
 | /  \ |
 |/    \|
 A------B

For example, at vertex A, the two angles are CAD, CAB.

We call such a quadrilateral for which all eight corner angles have
integer values when measured in degrees an "integer angled
quadrilateral". An example of an integer angled quadrilateral is a
square, where all eight corner angles are 45°. Another example is
given by DAC = 20°, BAC = 60°, ABD = 50°, CBD = 30°, BCA = 40°, DCA =
30°, CDB = 80°, ADB = 50°.

What is the total number of non-similar integer angled quadrilaterals?

Note: In your calculations you may assume that a calculated angle is
integral if it is within a tolerance of 10^-9 of an integer value.
-}

{-
quadrilateral with internal angles:
+--------+
|\a    b/|
|h\    /c|
|  \th/  |
|   \/   |
|   /\   |
|  /  \  |
|g/    \d|
|/f    e\|
+--------+


Constraint:
sin a * sin c * sin e * sin g
----------------------------- = 1
sin b * sin d * sin f * sin h

Constraints:
a+b = e+f
c+d = g+h
a+b+c+d = 180

assume wlog that:
theta <= 90
a <= b, e, f
if a = b then e <= f
if a = b = e = f then c <= d
-}

type R = Double
type Quad = (Int, Int, Int, Int, Int, Int, Int, Int)

quads :: R -> [(a, b, R)] -> [(c, d, R)] -> [(a, b, c, d)]
quads r [] ghs = []
quads r cds [] = []
quads r ((c,d,x):cds) ((g,h,y):ghs) =
  case compare1 (r * x * y) of
    LT -> quads r cds ((g,h,y):ghs)
    GT -> quads r ((c,d,x):cds) ghs
    EQ -> (c,d,g,h) : quads r cds ghs

compare1 :: R -> Ordering
compare1 x
  | x <= 1 - 1/(10^9) = LT
  | x >= 1 + 1/(10^9) = GT
  | otherwise = EQ

canonical :: Quad -> Bool
canonical q@(a,b,c,d,e,f,g,h) =
  all (q <=)
  [(b,a,h,g,f,e,d,c),
   (c,d,e,f,g,h,a,b),
   (d,c,b,a,h,g,f,e),
   (e,f,g,h,a,b,c,d),
   (f,e,d,c,b,a,h,g),
   (g,h,a,b,c,d,e,f),
   (h,g,f,e,d,c,b,a)]

quadrilaterals :: Int -> [Quad]
quadrilaterals n =
  [ (a,b,c,d,e,f,g,h) |
    a <- [1 .. n `div` 4],
    b <- [a .. n - 3*a],
    let ab = sine!a / sine!b,
    let efs = [ (e, f, sine!e / sine!f) |
                e <- [a .. b],
                let f = a + b - e],
    c <- [a .. n - 2*a - b],
    let d = n - a - b - c,
    a < c || b <= d,
    a < d || b <= c,
    let cd = sine!c / sine!d,
    let r = ab * cd,
    let ghs = [ (g, h, sine!g / sine!h) |
                h <- [a .. c + d - a], let g = c + d - h ],
    (e,f,g,h) <- quads r efs ghs ]
  where
    sine :: UArray Int R
    sine = listArray (0,n)
      [ sin (pi * fromIntegral a / fromIntegral n) | a <- [0 .. n] ]

prob177 :: Int -> Int
prob177 n = length (filter canonical (quadrilaterals n))

main :: IO String
main = return $ show $ prob177 180

-- Running time seems to be O(n^4)

answer :: String
answer = "129325"
