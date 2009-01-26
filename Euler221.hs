module Euler221 where
import qualified SortedList as S
import Primes

{-
Problem 221
13 December 2008

We shall call a positive integer A an "Alexandrian integer", if there
exist integers p, q, r such that:

A = p · q · r and

1/A = 1/p + 1/q + 1/r

For example, 630 is an Alexandrian integer (p = 5, q = −7, r =
−18). In fact, 630 is the 6th Alexandrian integer, the first 6
Alexandrian integers being: 6, 42, 120, 156, 420 and 630.

Find the 150000th Alexandrian integer.

-}

{-
A = pqr and 1/A = 1/p + 1/q + 1/r
1/pqr = 1/p + 1/q + 1/r

1 = qr + pr + pq
p^2 + 1 = p^2 + qr + pr + pq
p^2 + 1 = (p + q)(p + r)

Fact: p,q,r are pairwise coprime.

If A > 0, then exactly two of p,q,r must be negative.
WLOG, let p be the positive one.

p = (1 - qr)/(q + r)
q = (1 - pr)/(p + r)
r = (1 - pq)/(p + q)

If p>0, q<0, r<0, then pq<0, and (1-pq)>0. Thus (p+q)<0, i.e. p < -q.
Similarly, pr<0 and (1-pr)>0; thus (p+r)<0, i.e. p < -r.
Therefore p has the smallest absolute value of all three numbers.

We also have that -2p must be between q and r:
-2p <= q
-p <= p + q
(p + q)(p + r) <= -p(p + r)   (using p + r < 0)
p^2 + 1 <= -p(p + r)
p^2 < -p(p + r)
p < -(p + r)
p + r < -p
r < -2p

WLOG, assume that |q| < |r|. Then -q <= 2p < -r.

Furthermore, the only such solution with -q = 2p is (1,-2,-3).
-}

{-
1 = qr + pr + pq

This is a separated hyperboloid, symmetric about the axis p = q = r.

if (p,q,r) is a solution, then (-p, q+2p, r+2p) is also a solution.
Furthermore, (-p,-q,-r) is a solution, and any permutation of (p,q,r)
is a solution.

|-1 0 0|   |-1 0 0|
| 2 1 0| * | 2 1 0|
| 2 0 1|   | 2 0 1|

(p, q, r)
-----------
(1, -2, -3)
(2, -3, -7)
(3, -5, -8)
(3, -4, -13)
(4, -5, -21)
(5, -7, -18)
(5, -6, -31)

-q <= 2p < -r

We have -q <= 2p, thus 0 <= 2p + q = q'.
Inequality is strict unless (p,q,r) = (1,-2,-3).

We have 2p < -r, thus 0 > 2p + r = r'.

We have 0 < p, thus 0 > -p = p'.

-}

alexandrians :: [Integer]
alexandrians = f (1,-2,-3)
  where
    f (p,q,r) = p*q*r : S.union (f (next (q,p,r))) (f (next (r,p,q)))

next (p, q, r) = (-p, 2*p+q, 2*p+r)

prob221 :: Int -> Integer
prob221 n = alexandrians !! (n - 1)

main :: IO String
main = return $ show $ prob221 150000

answer :: String
answer = "1884161251122450"
