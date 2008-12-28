module Euler221 where
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
WLOG, assume that |q| < |r|.

Let Q = -q and R = -r.

1 = qr + pr + pq
1 = QR - pR - pQ
1 = QR - pR - pQ
1 + pQ = R(Q - p)
R = (1 + pQ)/(Q - p)

Let d = Q - p, so that Q = p + d.
R = (1 + p(p + d)) / d
R = (1 + p^2 + pd) / d
R - p = (1 + p^2) / d

Thus d must be a factor of 1 + p^2.

Any factor of p^2 + 1 is automatically coprime to p.



p(Q + R) = QR - 1



p(q + r) + qr = 1

qr == 1  (mod p)
pr == 1  (mod q)
pq == 1  (mod r)


---------------------

Table of values:

A  (p,q,r)
----------
6  (1, -2, -3)
42  (2, -3, -7)
120 (3, -5, -8)
156 (3, -4, -13)
420 (4, -5, -21)
630 (5, -7, -18)
930 (5, -6, -31)
...

-}


alexandrians =
  [ (p*q*r, (p, -q, -r)) |
    p <- [1 ..],
    d <- [1 .. p],
    let q = p + d,
    gcd p d == 1,
    let (e, z) = divMod (p*p + 1) d,
    z == 0,
    let r = p + e]

alexandrians' pmax =
  [ (p*q*r, (p, -q, -r)) |
    p <- [1 .. pmax],
    let k = p^2 + 1,
    let ds = list_divisors k,
    d <- takeWhile (<=p) ds,
    let e = k `div` d,
    let q = p + d,
    let r = p + e]
{-
Sorted by p, 150000 in the sequence has p = 28037.
The smallest
The last
88830221091882
22039139046653
length [ a | (a,_) <- alexandrians' 28037, a < 28037^3 ] = 33344
length [ a | (a,_) <- alexandrians' 30000, a < 30000^3 ] = 35776
length [ a | (a,_) <- alexandrians' 50000, a < 50000^3 ] = 60047
length [ a | (a,_) <- alexandrians' 70000, a < 70000^3 ] = 84389
length [ a | (a,_) <- alexandrians' 100000, a < 100000^3 ] = 121060
length [ a | (a,_) <- alexandrians' 130000, a < 130000^3 ] = 157923



-}


-- r = (1 - pq)/(p + q)
-- r = (1 - pq)/(p - q')
-- r' = (1 - pq)/(q' - p)
-- r' = (1 + p*q')/(q' - p)
