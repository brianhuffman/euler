module Euler318 where

{-

Problem 318
2011 nines

01 January 2011

Consider the real number √2+√3.
When we calculate the even powers of √2+√3 we get:
(√2+√3)^2 = 9.898979485566356...
(√2+√3)^4 = 97.98979485566356...
(√2+√3)^6 = 969.998969071069263...
(√2+√3)^8 = 9601.99989585502907...
(√2+√3)^10 = 95049.999989479221...
(√2+√3)^12 = 940897.9999989371855...
(√2+√3)^14 = 9313929.99999989263...
(√2+√3)^16 = 92198401.99999998915...

It looks like that the number of consecutive nines at the beginning of
the fractional part of these powers is non-decreasing. In fact it can
be proven that the fractional part of (√2+√3)^2n approaches 1 for
large n.

Consider all real numbers of the form √p+√q with p and q positive
integers and p<q, such that the fractional part of (√p+√q)^2n
approaches 1 for large n.

Let C(p,q,n) be the number of consecutive nines at the beginning of
the fractional part of (√p+√q)^2n.

Let N(p,q) be the minimal value of n such that C(p,q,n) ≥ 2011.

Find ∑N(p,q) for p+q ≤ 2011.

-}

{-

let r = √p√q
    r^2 = pq

Consider vectors (a,b), and define V(a,b) = a+br

(√p+√q)^0 = V(1, 0)
(√p+√q)^2 = V(p+q, 2)
(√p+√q)^4 = V(p^2+6pq+q^2, 4p+4q)
(√p+√q)^6 = V(p^3+15p^2q+15pq^2+q^3, 6p^2+20pq+6q^2)

(√p+√q)^2n = V(a,b) = a+br
(√p+√q)^(2n+2)
  = (a + br)(p+q + 2r)
  = a(p+q + 2r) + br(p+q + 2r)
  = (a(p+q) + 2ar) + (b(p+q) + 2br)r
  = (a(p+q)) + (2a + b(p+q))r + 2br^2
  = (a(p+q)) + (2a + b(p+q))r + 2bpq
  = (a(p+q) + 2b(pq)) + (2a + b(p+q))r
  = V((p+q)a + (2pq)b, 2a + (p+q)b)

Define T(a,b) = ((p+q)a + (2pq)b, 2a + (p+q)b)

V(T(x)) = (p+q+2r)*V(x)

T as a matrix:
|(p+q) (2pq)|   | a |
| (2)  (p+q)| * | b |

Eigenvectors
  a/b = r, eigenvalue = p+q+2r
  a/b = -r, eigenvalue = p+q-2r

The vectors i = (1/2, 1/2r) and j = (1/2, -1/2r) form an eigenbasis.
T(i) = (p+q+2r)i   V(i) = 1
T(j) = (p+q-2r)j   V(j) = 0

Initial vector = (1,0) = i + j.

Define W(a,b) = a-br.
We have W(i) = 0 and W(j) = 1.

For integers (a,b), W(a,b) will be close to an integer iff V(a,b) is.
T^n(1,0) is always a pair of integers.

W(T^n(1,0))
= W(T^n(i+j))
= W(T^n(i) + T^n(j))
= W((p+q+2r)^n*i + (p+q-2r)^n*j)
= (p+q+2r)^n*W(i) + (p+q-2r)^n*W(j)
= (p+q-2r)^n

W(T^n(1,0)) and V(T^n(1,0)) will tend to integers iff (p+q-2r) < 1.

For which p, q does this work?
p+q-2r < 1
p+q-1 < 2r
(p+q-1)^2 < (2r)^2
p^2+q^2+2pq-2p-2q+1 < 4pq
p^2+q^2-2pq-2p-2q+1 < 0
q^2 + (-2p-2)q + (p-1)^2 < 0
A = 1, B = (-2p-2), C = (p-1)^2
q : [ -B +/- sqrt(B^2 - 4AC) ] / 2A
q : [ 2p+2 +/- sqrt((2p+2)^2 - 4C) ] / 2
q : [ 2p+2 +/- sqrt(4(p+1)^2 - 4C) ] / 2
q : [ 2p+2 +/- 2 sqrt((p+1)^2 - C) ] / 2
q : p+1 +/- sqrt((p+1)^2 - C)
q : p+1 +/- sqrt((p+1)^2 - (p-1)^2)
q : p+1 +/- sqrt((p^2+2p+1) - (p^2-2p+1))
q : p+1 +/- sqrt(4p)
q : p+1 +/- 2 sqrt(p)
|q - (p+1)| < 2 sqrt(p)
|q - (p+1)|^2 < 4p
(q-p-1)^2 < 4p

-------------------------------------------------------

Let N(p,q) be the minimal value of n such that C(p,q,n) ≥ 2011.

The decimal expansion of (1-x) will have at least k nines if x < 1/10^k.

To have 2011 nines, we must have
(p+q-2r)^n < 1/10^2011
log((p+q-2r)^n) < log(1/10^2011)
n log(p+q-2r) < -2011 log(10)
n log(p+q-2r)/log(10) < -2011

n log(p+q-2r) < log(1/10^2011)

-}

log1e2011 :: Double
log1e2011 = -2011 * log 10

minimal_n :: Double -> Double -> Maybe Integer
minimal_n p q
  | e < 1 = Just (ceiling (log1e2011 / log e))
  | otherwise = Nothing
  where
    e = p + q - 2*sqrt(p*q)

mapJust :: (a -> Maybe b) -> [a] -> [b]
mapJust f [] = []
mapJust f (x : xs) =
  case f x of
    Just y -> y : mapJust f xs
    Nothing -> []

prob318 m = sum
  [ n | p <- [1..m], n <- mapJust (minimal_n p) [p+1..m-p] ]

main :: IO String
main = return $ show $ prob318 2011

answer :: String
answer = "709313889"
