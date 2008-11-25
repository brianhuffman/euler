module Euler143 where
import Pythagorean
import qualified SortedList as S
import List
import Array

------------------------------------------------------------------------------
-- 143. Investigating the Torricelli point of a triangle
{-
Let ABC be a triangle with all interior angles being less than 120 degrees.
Let X be any point inside the triangle and let XA = p, XB = q, and XC = r.

Fermat challenged Torricelli to find the position of X such that p + q + r was
minimised.

Torricelli was able to prove that if equilateral triangles AOB, BNC and AMC
are constructed on each side of triangle ABC, the circumscribed circles of AOB,
BNC, and AMC will intersect at a single point, T, inside the triangle. Moreover
he proved that T, called the Torricelli/Fermat point, minimises p + q + r. Even
more remarkable, it can be shown that when the sum is minimised, AN = BM = CO =
p + q + r and that AN, BM and CO also intersect at T.

If the sum is minimised and a, b, c, p, q and r are all positive integers we
shall call triangle ABC a Torricelli triangle. For example, a = 399, b = 455,
c = 511 is an example of a Torricelli triangle, with p + q + r = 784.

Find the sum of all distinct values of p + q + r <= 110000 for Torricelli
triangles.
-}

{-
(a,b,c) (p,q,r) (p+q+r)
(399,455,511) (195,264,325) (784)

length of chord, angle t, radius r:
2*r*sin(t/2)

Law of cosines:
c^2 = a^2 + b^2 - 2*a*b*cos(t)

For t = 120 degrees:
c^2 = a^2 + b^2 - 2*a*b*cos(120)
c^2 = a^2 + b^2 - 2*a*b*(-1/2)
c^2 = a^2 + b^2 + a*b
c^2 = (a+b)^2 - a*b

For Torricelli triangles:
a^2 = q^2 + r^2 + q*r
b^2 = p^2 + q^2 + p*q
c^2 = p^2 + r^2 + p*r

a^2 = (q+r)^2 - q*r
b^2 = (p+q)^2 - p*q
c^2 = (p+r)^2 - p*r

Problem:
Find (a,b,c) and (p,q,r) such that (q,r,a),
  (p,q,b), and (p,r,c) are Torricelli triples.
-}

torricelli_triangles m =
  [ (p,q,r) |
    p <- [1 .. m `div` 3],
    (q:qs) <- tails (arr!p),
    2*q <= m,
    r <- S.intersect qs (arr!q),
    p + q + r <= m ]
  where
    pairs = [ (a,b) | (a,b,c) <- torricelli_triples (2*m), 2*a <= m ]
    arr = accumArray (flip insert) [] (1, m `div` 2) pairs

prob143 n =
  sum $ S.nub $ sort $
  map (\(x,y,z) -> x+y+z) $ torricelli_triangles n

main :: IO String
main = return $ show $ prob143 110000
-- 25587759

-- TODO: clean this up

{-
prob143a m =
  [ (x,ys) |
    x <- [1 .. m-1],
    let ys = [ y | y <- [x+1 .. m], is_square ((x+y)^2 - x*y) ],
    not (null ys) ]
  where
    squareSet = Set.fromList [ n^2 | n <- [1 .. 2*m] ]
    is_square x = Set.member x squareSet

prob143b m =
  [ (x,y,z) |
    (x,ys) <- takeWhile ((<= m`div`3) . fst) $ f xs,
    let ys' = takeWhile (<= m-x) ys,
    (y,zs) <- takeWhile ((<= (m-x)`div`2) . fst) $ f ys',
    z <- takeWhile (<= m-x-y) zs ]
  where
    xs = [1 .. m]
    squareSet = Set.fromList [ x^2 | x <- [1 .. m] ]
    is_square x = Set.member x squareSet
    f [] = []
    f (x:xs) =
      let ys = filter (\y -> is_square ((x+y)^2 - x*y)) xs
      in if null ys then f xs else (x,ys) : f xs

prob143c = prob143b 110000

main = prob143
-}
