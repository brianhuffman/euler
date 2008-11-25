module Euler148 where
import EulerLib

{-
Problem 148
Exploring Pascal's triangle.

07 April 2007

We can easily verify that none of the entries in the first seven rows of
Pascal's triangle are divisible by 7:
      1
     1 1
    1 2 1
   1 3 3 1
  1 4 6 4 1
 1 5 10 10 5 1
1 6 15 20 15 6 1

However, if we check the first one hundred rows, we will find that only
2361 of the 5050 entries are not divisible by 7.

Find the number of entries which are not divisible by 7 in the first one
billion (10^9) rows of Pascal's triangle.
-}

{-
C(n,r) = n! / r! (n-r)!
P(a,b) = (a+b)! / a! b!

For prime p,
C(ap+b, cp+d) == C(a,c) * C(b,d)  (mod p)
P(ap+b, cp+d) == P(a,c) * P(b,d)  (mod p)

(p-1)! == (p-1)  (mod p)
(Product includes 1, p-1, other numbers paired with their inverses.)

(an+b)! = (product of multiples of n) * (product of non-multiples)
product of multiples = n^a * a!
product of non-mults == ((n-1)!)^a * b!  (mod n)

For n = prime p,
product of multiples = p^a * a!
product of non-mults == ((p-1)!)^a * b!  (mod p)
                     == (-1)^a * b!  (mod p)

map (map (flip mod 2)) pascal_triangle
rows 1,2,4,8,16,... contain 1s bracketing all 0s

map (map (flip mod 3)) pascal_triangle
rows 1,3,9,27... contain 1s bracketing all 0s

For prime p, row p^n of Pascal's triangle mod p
contains ones at the ends and all zeros in between.

Rows p^n .. 2(p^n)-1 contain two copies of
everything from rows 0 .. (p^n)-1.

Rows 2(p^n) .. 3(p^n)-1 contain three copies of
everything from rows 0 .. (p^n)-1.
...

for prime p: pT = triangle p = p(p+1)/2
first p^0 rows contain 1 non-multiple
first p^1 rows contain pT non-multiples
first p^2 rows contain pT^2 non-multiples
first p^n rows contain pT^n non-multiples

powers of 7 up to 1 billion:
7^10 = 282475249
-}

-- number of non-multiples of p in row n (zero-indexed)
count_row p 0 = 1
count_row p n = case divMod n p of
  (q, r) -> (r + 1) * count_row p q

-- number of non-multiples of p in first n rows
count_upto p n = case divMod n p of
  (0, r) -> triangle r
  (q, 0) -> triangle p * count_upto p q
  (q, r) -> triangle p * count_upto p q + triangle r * count_row p q

main :: IO String
main = return $ show $ count_upto 7 (10^9)
-- 2129970655314432
