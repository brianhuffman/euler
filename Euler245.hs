module Euler245 where
import Data.Ratio
import SquareRoot (square_root)
import Primes
import Permutation
import EulerLib
import Control.Monad.ST
import Data.Array.ST
import Data.Array

{-
Problem 245
Coresilience

15 May 2009

We shall call a fraction that cannot be cancelled down a resilient
fraction.  Furthermore we shall define the resilience of a
denominator, R(d), to be the ratio of its proper fractions that are
resilient; for example, R(12) = 4/11.

The resilience of a number d > 1 is then φ(d)/(d - 1), where φ is
Euler's totient function.

We further define the coresilience of a number n > 1 as

C(n) = (n - φ(n)) / (n - 1).

The coresilience of a prime p is C(p) = 1 / (p - 1).

Find the sum of all composite integers 1 < n ≤ 10^(11), for which C(n)
is a unit fraction.

-}

coresilience :: Integer -> Rational
coresilience n = (n - totient n) % (n - 1)

unit_fraction :: Rational -> Bool
unit_fraction x = numerator x == 1

-- up to 10^3: 6
-- up to 10^4: 10
-- up to 10^5: 22
-- up to 10^6: 

values =
  [ (n, denominator c, prime_factorization n) |
    n <- [3,5..],
    not (is_prime n),
    let c = coresilience n,
    unit_fraction c
  ]

{-

Case n = ab where a < b, both prime.

phi(n) = (a-1)(b-1)
n - phi(n) = a + b - 1

n - phi(n) | n - 1
a + b - 1  | ab - 1
a + b - 1  | a(a + b - 1) - (ab - 1)
a + b - 1  | a^2 + ab - a - ab + 1
a + b - 1  | a^2 - a + 1

Let f(a) = a^2 - a + 1
p | f(a) ==> p | f(a+d)
p | f(a) ==> p | f(1-a)    since f(1-a) = f(a)


k(n - phi(n)) = n - 1
k(a + b - 1) = ab - 1
(a-k)(a + b - 1) = a(a + b - 1) - (ab - 1)
(a-k)(a + b - 1) = a^2 + ab - a - ab + 1
(a-k)(a + b - 1) = a^2 - a + 1

---------------------------

a^2 - a + 1 == 0  (mod n)
2a == 1 +/- sqrt(-3)  (mod n)
n == 1 or 3 (mod 6)

---------------------------

Let n = a-k.

(a-k)(a + b - 1) = a^2 - a + 1
n(a + b - 1) = a^2 - a + 1
na + nb - n = a^2 - a + 1
nb = a^2 - (n+1)a + n + 1
nb = (a-1)(a-n) + 1
nb == (a-1)(a-n) + 1  (mod n)
0 == (a-1)a + 1  (mod n)
-1 == (a-1)a  (mod n)


-- Case n = 1 --

b = (a-1)^2 + 1
a*b = 3*5, 5*17, 7*37, 11*101, 13*(145), 17*257, ...

-- Case n = 3 --

3b = (a-1)(a-3) + 1
a == 2 (mod 3)

a*b = 5*3, 11*27, 17*75, 

-- Case n = 7 --

7b = (a-1)(a-7) + 1


a   f(a)
--------
3    7
5    21
7    43
11   111
17   273

a b
----
3 5
5 17
7 37
11 101
17 23
17 257
19 31
31 103
37 1297
41 1601
43 97
47 263
59 431
61 463
89 1031
101 173
101 677
107 491
211 397



----------------------------

-}

pf_array :: Integer -> Array Integer [(Integer, Int)]
pf_array m =
  runSTArray (do
    a <- (newArray_ (1, m) :: ST s (STArray s Integer Integer))
    b <- newArray (1, m) []
    -- a[i] <- f i
    mapM_ (\i -> writeArray a i (f i)) [1, 3 .. m]
    -- b[i] <- prime_factorization(f i)
    mapM_ (reduce a b 3) [5, 11 .. m]
    mapM_ (check a b) [3, 5 .. m]
    return b
  )
  where
    f i = i * (i-1) + 1
    -- invariant: odd i
    check a b i = do
      d <- readArray a i
      if d == 1 then return () else do
        mapM_ (reduce a b d) [i, i+2*d .. m]
        mapM_ (reduce a b d) [d+1-i, 3*d+1-i .. m]
    -- invariant: odd d, odd j
    reduce a b d j | j < 1 = return ()
    reduce a b d j = do
      x <- readArray a j
      let (x', e) = x `divN` d
      pf <- readArray b j
      writeArray a j x'
      writeArray b j ((d,e) : pf)

two_primes :: Integer -> [Integer]
two_primes nmax =
  [ p * q |
    p <- takeWhile (<= pmax) (tail primes),
    -- q + (p-1) | p^2 - p + 1
    x <- list_divisors_of_pf (a ! p),
    let q = x - p + 1,
    p < q,
    p * q <= nmax,
    is_prime q
  ]
  where
    pmax = square_root nmax
    a = pf_array pmax

--two_primes' :: Integer -> [Integer]
two_primes' nmax =
  [ (p, q) |
    p <- takeWhile (<= pmax) (tail primes),
    -- q + (p-1) | p^2 - p + 1
    x <- list_divisors_of_pf (a ! p),
    let q = x - p + 1,
    p < q,
    p * q <= nmax,
    is_prime q
  ]
  where
    pmax = square_root nmax
    a = pf_array pmax

-- length (two_primes (10^11)) = 3581

{-

(n - phi(n)) divides (n - 1).

k*n - k*phi(n) = n - 1
k*phi(n) == 1  (mod n)

phi(n) must be coprime to n.

Thus n must not have any repeated prime factors.
Also n must be odd.

Say n = p1*p2*...*pn, with i<j --> pi<pj.

Must check that pi does not divide (pj-1) for all i<j.

(Coprimality check leaves about 20% of all numbers as candidates.)

-}

{-

How big can the largest prime factor be?

Fix m, let n = mp, for large prime p.
let h = phi(m).

m p - phi(m p) | m p - 1
m p - (p-1) h | m p - 1
m p - p h + h | m p - 1
p (m - h) + h | m p - 1

k (p (m - h) + h) = m p - 1
k p (m - h) + k h = m p - 1
k h + 1 = p (m - k (m - h))
p = (k h + 1) / (m - k (m - h))

Both sides are positive, so
0 < m - k (m - h)
k (m - h) < m
k < m / (m - h)

p is maximized when k is as large as possible.

pmax = (k h + 1) / (m `mod` (m - h)), where k = m `div` (m - h)
pmax = (q h + 1) / r,  where (q, r) = m `divMod` (m - h)


------

15 p - phi(15 p) | 15 p - 1
15 p - 8 phi(p) | 15 p - 1
15 p - 8 (p - 1) | 15 p - 1
15 p - 8 p + 8 | 15 p - 1
7 p + 8 | 15 p - 1

k(7 p + 8) = (15 p - 1)
(7k) p + 8k = 15 p - 1

8k+1 = (15 - 7k) p

Both sides are positive.
Thus 0 <= k <= 2.
-------------------

Case n = p*q*r.

phi(n) = (p-1)(q-1)(r-1)

k (pqr - (p-1)(q-1)(r-1)) = (pqr - 1)
kpqr - k(p-1)(q-1)(r-1) = pqr - 1
kpqr - kp(q-1)(r-1) + k(q-1)(r-1) = pqr - 1
k(q-1)(r-1) + 1 = pqr + kp(q-1)(r-1) - kpqr
k(q-1)(r-1) + 1 = p (qr + k(q-1)(r-1) - kqr)

k phi(qr) + 1 = p (qr - k (qr - phi(qr)))

k m + 1 = p (m - k (m - h))

m - k (m - h) | k m + 1

97566000451 = 257 359 1057477



n - phi(n)
pqr - (p-1)(q-1)(r-1)
pqr - (pqr - (pq + pr + qr) + (p + q + r) - 1)
(pq + pr + qr) - (p + q + r) + 1  |  pqr - 1
pq + pr + qr - p - q - r + 1  |  pqr - 1
p(q+r) + qr - p - (q+r) + 1  |  pqr - 1
(p-1)(q+r) + qr - p + 1  |  pqr - 1
(p-1)(q+r) + qr - p + 1  |  p(p-1)(q+r) + pqr - pp + p
(p-1)(q+r) + qr - p + 1  |  p(p-1)(q+r) - pp + p + 1


p(q+r) + qr - p - (q+r) + 1  |  pp(q+r) + pqr - pp - p(q+r) + p
p(q+r) + qr - p - (q+r) + 1  |  pp(q+r) - pp - p(q+r) + p + 1


pq + pr + qr - p - q - r + 1  |  ppq + ppr + pqr - pp - pq - pr + p
pq + pr + qr - p - q - r + 1  |  ppq + ppr - pp - pq - pr + p + 1


(p,q) = (a-b, a+b)
p+q = 2a
p*q = (a-b)(a+b) = a^2 - b^2

2a - 1 | a^2 - b^2


Say n = p1*p2*p3.
phi(n) = (p1-1)(p2-1)(p3-1)



-}

{-

p0 <= (k*phi+1) / (n - k*(n-phi))
p0 * (n - k*(n-phi)) <= (k*phi+1)
p0*n - k*p0*(n-phi) <= k*phi+1
p0*n - 1 <= k*phi + k*p0*(n-phi)
p0*n - 1 <= k*(phi + p0*(n-phi))
(p0*n - 1) / (phi + p0*(n-phi)) <= k

-}


k_primes :: Int -> Integer -> [Integer]
k_primes k nmax = f k 1 1 (tail primes)
  where
    f 1 n phi (p0:_) = takeWhile (<= nmax)
      [ n*p |
        k <- [2,4 ..  n `div` (n - phi)],
        -- k*phi + 1 = p*(n - k*(n - phi))
        let (p, r) = divMod (k*phi+1) (n - k*(n-phi)),
        r == 0,
        p0 <= p,
        is_prime p,
        (p*n-1) `mod` (p*n - (p-1)*phi) == 0
      ]
    f k n phi (p : ps)
      | n*p^k > nmax = []
      | otherwise    =
          f (k-1) (n*p) (phi*(p-1)) ps ++
          f k n phi ps

all_primes :: Integer -> [Integer]
all_primes nmax =
  two_primes nmax ++
  k_primes 7 nmax ++
  k_primes 6 nmax ++
  k_primes 5 nmax ++
  k_primes 4 nmax ++
  k_primes 3 nmax

prob245 :: [Integer]
prob245 = all_primes (2*10^11)
--prob245 = two_primes (2*10^11)

main :: IO String
main = return $ show $ sum prob245

answer :: String
answer = "288084712410001"
