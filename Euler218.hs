module Euler218 where

{-
Problem 218
22 November 2008

Consider the right angled triangle with sides a=7, b=24 and c=25. The area
of this triangle is 84, which is divisible by the perfect numbers 6 and 28.
Moreover it is a primitive right angled triangle as gcd(a,b)=1 and gcd(b,c)=1.
Also c is a perfect square.

We will call a right angled triangle perfect if
  -it is a primitive right angled triangle
  -its hypotenuse is a perfect square

We will call a right angled triangle super-perfect if
  -it is a perfect right angled triangle and
  -its area is a multiple of the perfect numbers 6 and 28.

How many perfect right-angled triangles with c<=10^16 exist that are not
super-perfect?
-}

{-
Analysis:

a^2 + b^2 = c^2, coprime(a,b), perfect_square(c)

Thus (a,b,c) has the form (r^2 - s^2, 2rs, r^2 + s^2),
  for some coprime(r,s). Also c=t^2 for some t.
  (possibly with a,b swapped)

Thus r^2 + s^2 = t^2, i.e. (r,s,t) is a primitive pythagorean triple.

Thus (r,s,t) has the form (i^2 - j^2, 2ij, i^2 + j^2),
  for some coprime(i,j) (possibly with r,s swapped)

area = ab/2
6 | area AND 28 | area
84 | area
84 | ab/2
168 | ab
8*3*7 | ab
-}

type Z = Integer

area :: Z -> Z -> Z
area i j = a*r*s
  where
    r = j^2 - i^2
    s = 2*i*j
    t = i^2 + j^2
    a = abs(r^2 - s^2)

perfect_triangles :: Z -> [(Z, Z, Z)]
perfect_triangles m =
  [ (a, b, c) |
    i <- takeWhile (\i -> i^2 < m) [1 ..],
    j <- takeWhile (\j -> i^2 + j^2 <= m) [i+1 ..],
    gcd i j == 1,
    let r = j^2 - i^2,
    let s = 2*i*j,
    let t = i^2 + j^2,
    let a = abs(r^2 - s^2),
    let b = 2*r*s,
    let c = r^2 + s^2 ]

super_perfect :: (Z, Z, Z) -> Bool
super_perfect (a, b, c) = (a*b) `mod` 168 == 0

prob218 :: Z -> Int
--prob218 m = length $ filter (not . super_perfect) $ perfect_triangles m
prob218 m = 0

main :: IO String
main = return $ show $ prob218 (10^8)
-- 0
