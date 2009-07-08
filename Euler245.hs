module Euler245 where
import Data.Ratio
import SquareRoot (square_root)
import Primes
import Permutation
import EulerLib

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

--------------------------------------

Case n = p*q.

phi(n) = (p-1)*(q-1)


n - phi(n)            | n - 1
pq - (p-1)(q-1)       | pq - 1
pq - (pq - p - q + 1) | pq - 1
p + q - 1             | pq - 1

p + q - 1 | pq - 1
p + q - 1 | p^2 + pq - p
p + q - 1 | p^2 - p + 1
q + (p-1) | p^2 - p + 1

-------------------

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

coresilience :: Integer -> Rational
coresilience n = (n - totient n) % (n - 1)

unit_fraction :: Rational -> Bool
unit_fraction x = numerator x == 1

-- up to 10^3: 6
-- up to 10^4: 10
-- up to 10^5: 22
-- up to 10^6: 

values =
  [ (n, c, prime_factorization n) |
    n <- [3,5..],
    not (is_prime n),
    let c = coresilience n,
    unit_fraction c
  ]

two_primes :: Integer -> [Integer]
two_primes nmax =
  [ p*q |
    p <- takeWhile (<= square_root nmax) (tail primes),
    -- q + (p-1) | p^2 - p + 1
    x <- list_divisors (p^2 - p + 1),
    let q = x - p + 1,
    p < q,
    p * q <= nmax,
    is_prime q
  ]

-- length (two_primes (10^11)) = 3581

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

all_primes nmax =
  two_primes nmax ++
  k_primes 7 nmax ++
  k_primes 6 nmax ++
  k_primes 5 nmax ++
  k_primes 4 nmax ++
  k_primes 3 nmax

prob245 = all_primes (2*10^11)

-- 105788973228762 WRONG!
-- 288084712410001 RIGHT!


------------------

main :: IO String
main = return $ show $ sum prob245

answer :: String
answer = "288084712410001"


{-

3624 total.
Sum = 105788973228762

Two primes (4830):
         15 = 3 5
         85 = 5 17
        ...

Three primes (34 + 4...):
        255 = 3 5 17
      21845 = 5 17 257
     167743 = 43 47 83
     335923 = 7 37 1297
    3817309 = 19 31 6481
    3902867 = 53 211 349
    5574929 = 17 353 929
   10093049 = 83 277 439
   17632421 = 23 151 5077
   29087939 = 23 641 1973
   55762723 = 61 739 1237
  108197489 = 191 317 1787
  184912177 = 53 541 6449
  286114253 = 167 569 3011
  286331153 = 17 257 65537
 3415005551 = 53 877 73471
 3423145679 = 89 1061 36251
 9153262967 = 373 2473 9923
11936587651 = 293 307 132701
14531904773 = 433 1103 30427
15406265131 =
17401399289 = 59 1087 271333
17872005341 = 181 617 160033
21348695663 =
29709553147 = 79 6841 54973
34222741129 = 73 5521 84913
34659121499 = 47 2129 346373
36415854091 =
43072350809 = 181 947 251287
49637370089 = 2963 3251 5153
61453331837 = 179 4649 73847
64232837453 =
78160215977 = 541 6367 22691
97566000451 = 257 359 1057477
----------------------------
108461832103 =
117836861171 =
132550268551 =
143850557051 =


Four primes (7):
      65535 = 3 5 17 257
   27874645 = 5 17 353 929
 1431655765 = 5 17 257 65537
 1601953369 = 37 83 467 1117
23809671461 = 23 167 1879 3299
37013386447 = 43 59 61 239171
40955154631 = 17 23 4079 25679

Five primes (2):
   83623935 = 3 5 17 353 929
 4294967295 =

-}
