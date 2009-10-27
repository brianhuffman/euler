module Euler261 where
import SquareRoot
import Primes
import qualified SortedList as S

{---------------------------------------------------------------------
Problem 261
Pivotal Square Sums

23 October 2009

Let us call a positive integer k a square-pivot, if there is a pair of
integers m > 0 and n ≥ k, such that the sum of the (m+1) consecutive
squares up to k equals the sum of the m consecutive squares from (n+1)
on:

(k-m)^(2) + ... + k^(2) = (n+1)^(2) + ... + (n+m)^(2).

Some small square-pivots are

    * 4: 3^(2) + 4^(2) = 5^(2)
    * 21: 20^(2) + 21^(2) = 29^(2)
    * 24: 21^(2) + 22^(2) + 23^(2) + 24^(2) = 25^(2) + 26^(2) + 27^(2)
    * 110: 108^(2) + 109^(2) + 110^(2) = 133^(2) + 134^(2)

Find the sum of all distinct square-pivots ≤ 10^(10).

---------------------------------------------------------------------}

{---------------------------------------------------------------------

(SUM i:[1..n]. i^2) = (2n^3 + 3n^2 + n) / 6

Define s(x) = 2x^3 + 3x^2 + x

(k,m,n) is a solution if
s(n+m) - s(n) = s(k) - s(k-m-1)
km + mn + kmm + mmn + mnn = kk + kkm
km + mn + kmm + mmn + mnn - kkm = kk
m(k + n + km + mn + nn + kk) = kk
m(n + k)(1 + m + n - k) = k^2

A good solution is a triple (k,m,n) such that
0 < m, 0 < k <= n, and k^2 = m(n+k)(1+m+n-k)

A ok solution is a triple (k,m,n) such that
0 < m, 0 < k, 0 < n, and k^2 = m(n+k)(1+m+n-k)

----------------------------------------------------------------------
Degenerate solutions (k = 0)

m(n + k)(1 + m + n - k) = k^2
mn(1 + m + n) = 0
Either m = 0, or n = 0, or m+n+1 = 0.

(0,0,n)
(0,m,0)
(0,m,-m-1)

----------------------------------------------------------------------
Solution transformations

swap: (k,m,n) -> (n, m+n-k, k)

next: (k,m,n) -> (k+m+2m(k+n), m, n+2k+2m(k+n))

prev: (k,m,n) -> (k+m(2k-2m-2n-1), m, n-2k-2m(k-m-n-1))

(k'+m'+2m'(k'+n'), m', n'+2k'+2m'(k'+n'))
k' = n
m' = (m+n-k)
n' = k
k'' = n+(m+n-k)+2(m+n-k)(n+k)
    = 2n+m-k+2(m+n-k)n+2(m+n-k)k
    = 2n+m-k+(2m+2n-2k)n+(2m+2n-2k)k
    = 2n+m-k+2mn+2nn-2kn+2mk+2nk-2kk
    = 2n+m-k+2mn+2nn+2mk-2kk
    = 2n+(m-k)+2n(m+n)+2k(m-k)
    = 2n(m+n+1)+(2k+1)(m-k)
m'' = m+n-k
n'' = k+2n+2(m+n-k)(n+k)


* next maps a good solution to a larger good solution.

* 



next: (k,m,d) -> (k+m(4k+2d+1), m, d+2k-m)
prev: (k,m,d) -> (k-m(2d+2m+1), m, d-2k+m(4d+4m+3))
conv: (k,m,d) -> (k+d, m+d, -d)
turn: (k,m,d) -> (k, -(m+d+1), d)

k
Either (k and m both even) OR (m odd and n even)

For n >= k:
k^2 = m(n+k)(m+1+n-k)
k^2 >= m(2k)(m+1)
k^2 > 2km^2
k > 2m^2
m < sqrt (k / 2)

-- Change of variables --

For n = k+d:
k^2 = m(n+k)(m+1+n-k)
k^2 = m(2k+d)(m+1+d)

Solution is a triple (k,m,d) such that
m > 0, d >= 0, and k^2 = m(2k+d)(m+1+d)

k^2 = m(2k+d)(m+1+d)
k^2 > m(2k)(m)
k^2 > 2km^2
k > 2m^2
m <= square_root (k / 2)

--------------------

(k-m)^(2) + ... + k^(2) = (k+d+1)^(2) + ... + (k+d+m)^(2).

Add (k+1)^(2) + ... (k+d)^(2) to both sides:

(k-m)^(2) + ... + (k+d)^(2) = (k+1)^(2) + ... + (k+d+m)^(2).

(k,m,d) -> (k+d, m+d, -d)
(operation is self-inverse)


--------------------
Solution families:

(                 2m^2 + 2m, m,                            0)
(         8m^3 + 10m^2 + 3m, m,                   4m^2 +  3m)
(32m^4 + 56m^3 + 28m^2 + 4m, m,          16m^3 + 24m^2 +  8m)
...

Solution transformations:

(0,0,d)
(0,m,0)
(0,m,-m-1)
next: (k,m,d) -> (k+m(4k+2d+1), m, d+2k-m)
prev: (k,m,d) -> (k-m(2d+2m+1), m, d-2k+m(4d+4m+3))
conv: (k,m,d) -> (k+d, m+d, -d)
turn: (k,m,d) -> (k, -(m+d+1), d)

next . prev = id
prev . next = id
conv . conv = id
turn . turn = id

Case k = 0:
  k^2 = m(2k+d)(m+1+d)
  0 = md(m+1+d)
  m = 0 | d = 0 | m+1+d = 0



Given a solution (k,m,d), with positive k and m, how can we transform
it to another solution (k',m',d') with k' < k?

* If d < 0, apply conv. (k+d < k)

  Does k stay positive?
  0 <= k+d (invariant)

  Does m stay positive?
  0 <= m+d (invariant)

* If d >= 0, apply prev.

  Does k get smaller?
  k - m(2d + 2m + 1) < k
  0 < m(2d + 2m + 1)
  0 < 2d + 2m + 1
  YES.

  Does k stay positive?
  0 <= k - m(2d + 2m + 1)
  m(2d + 2m + 1) <= k
  km(2d + 2m + 1) <= k^2
  km(2d + 2m + 1) <= m(2k+d)(m+1+d)
  k(2d + 2m + 1) <= (2k+d)(m+1+d)
  2kd + 2km + k <= 2km + 2k + 2kd + md + d + d^2
  0 <= k + md + d + d^2
  YES.

  Does k+d stay positive?
  k' + d' = k-m(2d+2m+1) + d-2k+m(4d+4m+3)
  k' + d' = d - k + 2m(m+1+d)
  0 <= d - k + 2m(m+1+d)
  0 <= (2k+d)d - (2k+d)k + 2m(2k+d)(m+1+d)
  0 <= (2k+d)d - (2k+d)k + 2k^2
  0 <= 2dk + d^2 - 2k^2 - dk + 2k^2
  0 <= dk + d^2
  YES.

  Does m+d stay positive?
  m' + d' = m + d-2k+m(4d+4m+3)
  0 <= m + d - 2k + m(4d+4m+3)
  0 <= d - 2k + m(4d+4m+4)
  0 <= d - 2k + 4m(m+1+d)
  0 <= d(2k+d) - 2k(2k+d) + 4m(2k+d)(m+1+d)
  0 <= d(2k+d) - 2k(2k+d) + 4k^2
  0 <= d^2
  YES.

-}

type Triple = (Integer, Integer, Integer)

is_solution :: Triple -> Bool
is_solution (k,m,d) = k^2 == m*(2*k+d)*(m+1+d)

-- is_solution (basic m)
basic :: Integer -> Triple
basic m = (2*m^2 + 2*m, m, 0)

-- is_solution (k,m,d) ==> is_solution (next (k,m,d))
next :: Triple -> Triple
next (k,m,d) = (k+m*(4*k+2*d+1), m, 2*k+d-m)

-- prev (next (k,m,d)) = (k,m,d)
-- next (prev (k,m,d)) = (k,m,d)
prev :: Triple -> Triple
prev (k,m,d) = (k-m*(2*d+2*m+1), m, d-2*k+m*(4*d+4*m+3))

conv :: Triple -> Triple
conv (k,m,d) = (k+d, m+d, -d)

-- brute force search
kmds :: [Triple]
kmds =
  [ (k, m, d) |
    k <- [1 ..],
    let pf = prime_factorization k,
    let pf2 = [ (p, e*2) | (p, e) <- pf ],
    m <- takeWhile (\m -> 2*m^2 < k) (list_divisors_of_pf pf2),
    let q = (k^2) `div` m,
    let x = (2*k-m-1)^2 + 4*q,
    let (t, s) = square_root_aux x,
    s == 0,
    even (t-2*k-m-1),
    let d = (t-2*k-m-1) `div` 2
  ]

prune_tree :: Integer -> Triple -> [Integer]
prune_tree z t@(k,_,_)
  | k > z = []
  | otherwise = k : S.union (prune_tree z (next t))
                            (prune_tree z (next (conv t)))

all_upto :: Integer -> [Integer]
all_upto z =
  foldr S.union []
    (takeWhile (not . null) [ prune_tree z (basic m) | m <- [1 ..] ])

main :: IO String
main = return $ show $ sum $ all_upto (10^10)

answer :: String
answer = "238890850232021"
