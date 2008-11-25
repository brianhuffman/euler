module Euler195 where

{-
Problem 195
Inscribed circles of triangles with one angle of 60 degrees.

23 May 2008

Let's call an integer sided triangle with exactly one angle of 60 degrees
a 60-degree triangle. Let r be the radius of the inscribed circle of such
a 60-degree triangle.

There are 1234 60-degree triangles for which r <= 100.
Let T(n) be the number of 60-degree triangles for which r <= n,
so T(100) = 1234, T(1000) = 22767, and T(10000) = 359912.

Find T(1053779).
-}

{-
radius of inscribed circle:
s = (a+b+c)/2
r = (s-c) * tan (C/2)

area of triangle = bh/2, where h = a*sin 60 = sqrt 3 / 2
area of triangle = a*b * sqrt3 / 4
radius of inscribed circle
  = area / ((a+b+c)/2)
  = (a*b * sqrt3 / 4) / ((a+b+c)/2)
  = (a*b * sqrt3) / 2(a+b+c)
(a*b * sqrt3) / 2(a+b+c) <= rmax
a*b * sqrt3 <= rmax * 2 * (a+b+c)
a^2*b^2 * 3 <= rmax^2 * 4 * (a+b+c)^2

radius of inscribed circle = (a+b-c)/2 * tan 30 = (a+b-c) * sqrt 3 / 6
(a+b-c)/2 * sqrt 3 / 3 <= rmax
(a+b-c) * sqrt 3 <= 6 * rmax
(a+b-c)^2 * 3 <= 36 * rmax^2
(a+b-c)^2 <= 12 * rmax^2
(a+b-c) <= sqrt 12 * rmax

r <= 100 iff (a+b-c) <= 346
r <= 1000 iff (a+b-c) <= 3464
r <= 10000 iff (a+b-c) <= 34641
r <= 1053779 iff (a+b-c) <= 3650397
-}


--------------------------------------------------

{-
Law of cosines:
a^2 + b^2 - 2ab cos theta = c^2

For theta = 60 degrees, cos theta = 1/2.

a^2 + b^2 - a*b = c^2

Each solution (a,b,c) has a complement solution (b-a,b,c).

      /\
     /60\(a)
    /   _\
(b)/  _-  \
  / _-(c)  \(b-a)
 /_-        \
/-_________60\
     (b)

if a^2 + b^2 - a*b = c^2, for a < b and coprime a b,
then (a,b,c) must be of the form
  A) (q^2 - p^2, q^2 + 2*p*q, q^2 + p^2 + p*q)
 OR
  B) (p^2 + 2*p*q, q^2 + 2*p*q, q^2 + p^2 + p*q)  (complemented)
for p < q, coprime.

The only possible common factor of (a,b,c) is 3;
this happens iff q-p is a multiple of 3.
In this case, the complement of the reduced pair is generated
 by a smaller p', q':
triple (3i) (3j) = 9 * triple i j = 3 * triple' (j-i) (2i+j)
triple (3i+1) (3j+1) ~~ 3 * triple' (j-i) (2i+j+1)
triple (3i+2) (3j+2) ~~ 3 * triple' (j-i) (2i+j+2)

--- Derivation ---

a^2 + b^2 - a*b = c^2
(a/c)^2 + (b/c)^2 - (a/c)(b/c) = 1

Equivalent to finding rational solutions to x^2 + y^2 - xy = 1
Positive rational solutions are 1-to-1 with rational slopes:
  m = -slope of (0,1)-(x,y) = (1-y) / x,  0 <= m < 1

y = 1 - mx
x^2 - xy + y^2 = 1
x^2 - x(1 - mx) + (1 - mx)^2 = 1
x^2 - x + mx^2 + 1 - 2mx + m^2x^2 = 1
x^2 - x + mx^2 - 2mx + m^2x^2 = 0
(1 + m + m^2)x^2 + (-1 - 2m)x = 0
(1 + m + m^2)x + (-1 - 2m) = 0    (assume x <> 0)
(1 + m + m^2)x = 1 + 2m
x = (1 + 2m) / (1 + m + m^2)

y = 1 - mx
y = 1 - m(1 + 2m) / (1 + m + m^2)
y = 1 - (m + 2m^2) / (1 + m + m^2)
y = (1 + m + m^2 - m - 2m^2) / (1 + m + m^2)
y = (1 - m^2) / (1 + m + m^2)

m = p/q

x = (1 + 2(p/q)) / (1 + (p/q) + (p/q)^2)
y = (1 - (p/q)^2) / (1 + (p/q) + (p/q)^2)

x = (q^2 + 2pq) / (q^2 + pq + p^2)
y = (q^2 - p^2) / (q^2 + pq + p^2)

Case A)
  a = q^2 - p^2
  b = q^2 + 2pq
  c = q^2 + pq + p^2
  a+b = 2q^2 + 2pq - p^2
  a+b+c = 3*q^2 + 3pq = 3*q*(p+q)
  a+b-c = q^2 + pq - 2p^2 = (q+2p)(q-p) = (q-p)((q-p)+3p)
  a+b-c = d(d+3p), where d = q-p

Case B)
  a = p^2 + 2pq
  b = q^2 + 2pq
  c = q^2 + pq + p^2
  a+b = q^2 + 4pq + p^2
  a+b+c = 2*q^2 + 5*p*q + 2*p^2
  a+b-c = 3pq
  let d = q-p, q = p+d
  a+b-c = 3p(p+d)
-}

-- triple_of_ratio (-p) (p+q)
--  = (\(a,b,c) -> (b,a,c)) (triple_of_ratio p q)
-- triple_of_ratio (q-p) (2p+q)
--  = (\(a,b,c) -> (3(b-a), 3b, 3c)) (triple_of_ratio p q)

type Z = Int

triple_of_ratio :: Z -> Z -> (Z, Z, Z)
triple_of_ratio p q = (a, b, c)
  where
    a = q^2 - p^2
    b = q^2 + 2*p*q
    c = q^2 + p^2 + p*q

triple_of_ratio' :: Z -> Z -> (Z, Z, Z)
triple_of_ratio' p q = (a, b, c)
  where
    a = p^2 + 2*p*q
    b = q^2 + 2*p*q
    c = q^2 + p^2 + p*q

-- all solutions to a^2 + b^2 - ab = c^2
-- with a < b, coprime a b, a+b-c <= n
primitive_triples :: Z -> [(Z, Z, Z)]
primitive_triples m =
  [ triple_of_ratio p q |
    d <- takeWhile (\d -> d^2 <= m) [1 ..],
    d `mod` 3 /= 0,
    p <- takeWhile (\p -> d*(d+3*p) <= m) [1 ..],
    let q = p + d,
    gcd p q == 1 ]

primitive_triples' :: Z -> [(Z, Z, Z)]
primitive_triples' m =
  [ triple_of_ratio' p q |
    p <- takeWhile (\p -> 3*p^2 <= m) [1 ..],
    q <- takeWhile (\q -> 3*p*q <= m) [p+1 ..],
    (p - q) `mod` 3 /= 0,
    gcd p q == 1 ]

-- [ a+b-c | (a,b,c) <- primitive_triples m ]
primitive_sums :: Z -> [Z]
primitive_sums m =
  [ x |
    d <- takeWhile (\d -> d^2 <= m) [1 ..],
    d `mod` 3 /= 0,
    let xs = [ d*(d+3*p) | p <- [1 ..], gcd p d == 1 ],
    x <- takeWhile (<=m) xs ]

-- [ a+b-c | (a,b,c) <- primitive_triples' m ]
primitive_sums' :: Z -> [Z]
primitive_sums' m =
  [ x |
    p <- takeWhile (\p -> 3*p^2 <= m) [1 ..],
    let xs = [ 3*p*q | q <- [p+1 ..],
               (p - q) `mod` 3 /= 0, gcd p q == 1 ],
    x <- takeWhile (<= m) xs ]

num_triples :: Z -> Z
--num_triples m = sum [ m `div` (a+b-c) | (a,b,c) <- primitive_triples m ]
num_triples m = sum [ m `div` r | r <- primitive_sums m ]

num_triples' :: Z -> Z
--num_triples' m = sum [ m `div` (a+b-c) | (a,b,c) <- primitive_triples' m ]
num_triples' m = sum [ m `div` r | r <- primitive_sums' m ]

all_triples :: Z -> [(Z, Z, Z)]
all_triples m =
  [ (a*k, b*k, c*k) |
    (a,b,c) <- primitive_triples m,
    k <- [1 .. m `div` (a+b-c)] ]

prob195 :: Z -> Z
prob195 rmax = num_triples m + num_triples' m
  where m = floor (sqrt 12 * fromIntegral rmax)

main :: IO String
main = return $ show $ prob195 1053779

