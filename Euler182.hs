module Euler182 where

{-
Problem 182
15 February 2008

The RSA encryption is based on the following procedure:

Generate two distinct primes p and q.
Compute n=pq and φ=(p-1)(q-1).
Find an integer e, 1 < e < φ, such that gcd(e,φ)=1.

A message in this system is a number in the interval [0,n-1].
A text to be encrypted is then somehow converted to messages (numbers
in the interval [0,n-1]).
To encrypt the text, for each message, m, c = m^e mod n is calculated.

To decrypt the text, the following procedure is needed: calculate d such that
ed = 1 mod φ, then for each encrypted message, c, calculate m = c^d mod n.

There exist values of e and m such that m^e mod n = m.
We call messages m for which m^e mod n = m unconcealed messages.

An issue when choosing e is that there should not be too many unconcealed
messages.
For instance, let p=19 and q=37.
Then n=19*37=703 and φ=18*36=648.
If we choose e=181, then, although gcd(181,648)=1 it turns out that all
possible messages m (0 <= m <= n-1) are unconcealed when calculating m^e mod n.
For any valid choice of e there exist some unconcealed messages.
It's important that the number of unconcealed messages is at a minimum.

Choose p=1009 and q=3643.
Find the sum of all values of e, 1<e<φ(1009,3643) and gcd(e,φ)=1, so that
the number of unconcealed messages for this value of e is at a minimum.
-}

{-
Choose p=1009 and q=3643.
Find the sum of all values of e, 1<e<φ(1009,3643) and gcd(e,φ)=1,
so that the number of unconcealed messages (m where m^e mod n=m)
for this value of e is at a minimum.

n = 1009*3643 = 3675787
φ = 1008*3642 = 3671136 = 2^5 * 3^3 * 7 * 607
number of values of e = 1047168 = 2^7 * 3^4 * 101

m^e == m  (mod pq)
iff
m^e == m  (mod p)
and
m^e == m  (mod q)

Determining the number of unconcealed messages:

m^e == m  (mod p)
p | m^e - m
p | m * (m^(e-1) - 1)
Thus p | m -OR- p | m^(e-1)-1.
Case 1: m == 0 (mod p)        (1 solution)
Case 2: m^(e-1) == 1 (mod p)  (gcd (e-1) (p-1) solutions)

The total number of unconcealed messages is given by
1 + gcd (e-1) (p-1)

For any prime modulus p > 2 and exponent e coprime to (p-1), there are
always at least 3 unconcealed messages m: -1, 0, 1.
-}

good_exponents :: Int -> Int -> [Int]
good_exponents p q =
  [ e |
    e <- [3, 5 .. phi-1],
    gcd phi e == 1,
    gcd (e-1) (p-1) == 2,
    gcd (e-1) (q-1) == 2 ]
  where
    phi = (p-1) * (q-1)

prob182 :: Int -> Int -> Integer
prob182 p q = sum [ toInteger e | e <- good_exponents p q ]

main :: IO String
main = return $ show $ prob182 1009 3643
-- 399788195976
-- length (good_exponents 1009 3643) = 217800
