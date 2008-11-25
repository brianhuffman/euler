module Euler188 where
import Primes

{-
Problem 188
04 April 2008

The hyperexponentiation or tetration of a number a by a positive integer b,
denoted by a^^b or ^b_a, is recursively defined by:

a^^1 = a,
a^^(k+1) = a^(a^^k).

Thus we have e.g. 3^^2 = 3^3 = 27, hence 3^^3 = 3^27 = 7625597484987 and
3^^4 is roughly 10^(3.6383346400240996*10^12).

Find the last 8 digits of 1777^^1855.
-}

{-
Analysis:

Goal: calculate a^^b (mod 10^k).

Assume that a is coprime to 10^k.

Define G(m) = the greatest order of any element
in the multiplicative group of integers mod m.

For coprime a b, G(a*b) = lcm(G(a), G(b))

G(2) = 1
G(4) = 2
G(2^k) = 2^(k-2), for k >= 3

G(5^k) = totient (5^k) = 4 * 5^(k-1)

G(10) = lcm(G(2), G(5)) = lcm(1, 4) = 4
G(100) = lcm(G(4), G(25)) = lcm(2, 20) = 20
G(1000) = lcm(G(8), G(125)) = lcm(2, 100) = 100
G(20) = lcm(G(4), G(5)) = lcm(2, 4) = 4

For k > 3, G(10^k)
= lcm(G(2^k), G(5^k))
= lcm(2^(k-2), 4 * 5^(k-1))
= 2^(k-2) * 5^(k-1)
= 5 * 10^(k-2)

G(2^k)
The greatest multiplicative order...
... mod 2^k is 2^(max 1 (k-2)).
... mod 5^k is 4 * 5^(k-1)
... mod 10^k is lcm (2^(max 1 (k-2)), 4 * 5^(k-1))
              = 2^(max 2 (k-2)) * 5^(k-1)

100000000
  5000000
   250000
    12500
     2500
      500
      100
       20
        4
        2
-}

greatest_order n = foldl lcm 1 (map f pf)
  where
    pf = prime_factorization n
    f (2, e) = 2^(max 1 (e-2))
    f (p, e) = (p-1) * p^(e-1)

-- a^^b (mod n)
tetration_mod a b n
  | b == 1 = a `mod` n
  | n == 2 = if even a then 0 else 1
  | otherwise = expMod a k n
  where
    m = greatest_order n
    k = tetration_mod a (b-1) m

main :: IO String
main = return $ show $ tetration_mod 1777 1855 (10^8)
