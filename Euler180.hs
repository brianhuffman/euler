module Euler180 where
import EulerLib
import Ratio
import List
import qualified Data.Set as Set
import qualified Data.Map as Map

{-
Problem 180
Rational zeros of a function of three variables.

02 February 2008

For any integer n, consider the three functions
 f1,n(x,y,z) = x^(n+1) + y^(n+1) − z^(n+1)
 f2,n(x,y,z) = (xy + yz + zx)*(x^(n-1) + y^(n-1) − z^(n-1))
 f3,n(x,y,z) = xyz*(x^(n-2) + y^(n-2) − z^(n-2))
and their combination
fn(x,y,z) = f1,n(x,y,z) + f2,n(x,y,z) − f3,n(x,y,z)

We call (x,y,z) a golden triple of order k if x, y, and z are all rational
numbers of the form a / b with 0 < a < b ≤ k and there is (at least) one
integer n, so that fn(x,y,z) = 0.

Let s(x,y,z) = x + y + z.

Let t = u / v be the sum of all distinct s(x,y,z) for all golden triples
(x,y,z) of order 35.
All the s(x,y,z) and t must be in reduced form.

Find u + v.
-}


{-
Analysis:

fn(x,y,z) =
  x^(n+1) + y^(n+1) − z^(n+1)
  + (xy + yz + zx)*(x^(n-1) + y^(n-1) − z^(n-1))
  − xyz*(x^(n-2) + y^(n-2) − z^(n-2))

fn(x,y,z) =
  x^(n+1) + y^(n+1) − z^(n+1)
  + xy*x^(n-1) + yz*x^(n-1) + zx*x^(n-1)
  + xy*y^(n-1) + yz*y^(n-1) + zx*y^(n-1)
  − xy*z^(n-1) − yz*z^(n-1) − zx*z^(n-1)
  − xyz*x^(n-2) − xyz*y^(n-2) + xyz*z^(n-2)

fn(x,y,z) =
  x^(n+1) + y^(n+1) − z^(n+1)
  + y*x^n + yz*x^(n-1) + z*x^n
  + x*y^n + z*y^n + zx*y^(n-1)
  − xy*z^(n-1) − y*z^n − x*z^n
  − yz*x^(n-1) − xz*y^(n-1) + xy*z^(n-1)

fn(x,y,z) =
  x^(n+1) + y^(n+1) − z^(n+1)
  + y*x^n + z*x^n + x*y^n
  + z*y^n − y*z^n − x*z^n

fn(x,y,z) = (x+y+z)*x^n + (x+y+z)*y^n − (x+y+z)*z^n

fn(x,y,z) = (x+y+z)*(x^n + y^n − z^n)

x, y, z are all strictly positive.

fn(x,y,z) = 0  iff  (x^n + y^n = z^n)

(a/b)^n + (c/d)^n = (e/f)^n
(a^n/b^n) + (c^n/d^n) = (e^n/f^n)
a^n d^n f^n + b^n c^n f^n = b^n d^n e^n
(adf)^n + (bcf)^n = (bde)^n

By Fermat's last theorem, this can only hold for n=[-2,-1,1,2].

Note that (x,y,z) is golden iff (y,x,z) is golden.
Also, s(x,y,z) = s(y,x,z).
So we only need to consider (x,y,z) such that x <= y.

s(x,y,z)
= x + y + z
= a/b + c/d + e/f
= adf/bdf + bcf/bdf + bde/bdf
= (adf + bcf + bde) / bdf
-}

farey_sequence n = fs 1 n 1 (n-1)
  where
    fs a b c d
      | c >= n = []
      | otherwise = (a, b) : fs c d i j
      where
        k = (n + b) `div` d
        i = k * c - a
        j = k * d - b

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs@(x : xs') = [ (x, y) | y <- xs ] ++ pairs xs'

golden_triples :: Integer -> [(Rational, Rational, Rational)]
golden_triples k = -- 6851 distinct
  [ (x, y, z) |
    ((a, b), (c, d)) <- pairs fracs,
    let x = a % b,
    let y = c % d,
    op <- [f1, f2, f3, f4],
    z <- op x y,
    z < 1,
    denominator z <= k ]
  where
    fracs = farey_sequence k
    sqrts = Map.fromList [ (n^2, n) | n <- [1 .. k^2] ]
    is_square x = Map.lookup x sqrts
    f1 x y =
      let w = x^2 + y^2
      in [ a % b | a <- is_square (numerator w),
                   b <- is_square (denominator w) ]
    f2 x y = [x + y]
    f3 x y = [recip (recip x + recip y)]
    f4 x y = map recip (f1 (recip x) (recip y))

sum_nub :: [Rational] -> Rational
sum_nub = Set.fold (+) 0 . Set.fromList

prob180 k = u + v
  where
    s (x, y, z) = x + y + z
    t = sum_nub $ map s $ golden_triples k
    u = numerator t
    v = denominator t
 -- 3677 distinct

main :: IO String
main = return $ show $ prob180 35
-- 285196020571078987

-- TODO: rewrite this algorithm using Integer instead of Rational
-- i.e. use the common denominator d = fold lcm [1..k]
