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


------------ More changes of variables -------------

2m+d+1 -> negated.
2k+d -> same.
d -> same.

2m+d+1 + 2m'+d+1 = 0

2m'+d+1 = -(2m+d+1)
2m'+d+1 = -2m - d - 1
m' = -(m+d+1)

2m+d+1 -> negated.


k^2 = m(2k+d)(m+1+d)

Let x = 2k+d, 2k=(x-d)
Let y = 2m+d, 2m=(y-d)

k^2 = m(2k+d)(m+1+d)
4k^2 = 4m(2k+d)(m+1+d)
(2k)^2 = 2m(2k+d)(2m+2+2d)
(x-d)^2 = (y-d)(x-d+d)(y-d+2+2d)
(x-d)^2 = (y-d)(x)(y+d+2)
x^2 + d^2 - 2xd = (xy-xd)(y+d+2)
x^2 + d^2 - 2xd = xy(y+d+2) - xd(y+d+2)
x^2 + d^2 - 2xd = xy^2 + xyd + 2xy - xyd - xd^2 - 2xd
x^2 + d^2 = xy^2 + 2xy - xd^2
x^2 + d^2 + xd^2 = xy^2 + 2xy
x^2 + d^2 + xd^2 + x = xy^2 + 2xy + x
x^2 + d^2 + xd^2 + x = x(y^2 + 2y + 1)
x^2 + d^2 + xd^2 + x = x(y+1)^2

Let v = y+1.

x^2 + d^2 + xd^2 + x = xv^2

x^2 + d^2 + xd^2 + x - xv^2 = 0
x^2 + x(d^2 - v^2 + 1) + d^2 = 0
4x^2 + 4x(d^2 - v^2 + 1) + 4d^2 = 0

Let u = (d^2 - v^2 + 1)

4x^2 + 4xu + 4d^2 = 0
4x^2 + 4xu + u^2 + 4d^2 = u^2
(2x+u)^2 + 4d^2 = (d^2 - v^2 + 1)^2

Let w = 2x+u

w^2 + 4d^2 = (d^2 - v^2 + 1)^2
w^2 + 4d^2 = (d^2 - v^2 + 1)^2









* If d = 0, then k = 2m(m+1), and prev gives (m, m, -m).


k^2 = m(2k+d)(m+1+d)



Other solutions:

(28,8,-7) -> (820,8,41) -> (27724,8,1673) -> (941668,8,57113)
(132,24,-22) -> (11772,24,218) -> (1152372,24,23738)
(168,49,-48) -> (28441,49,239)
(360,48,-45) -> (65208,48,627)
(861,49,-41) -> (165648,49,1632)
(760,80,-76) -> (231880,80,1364)
(1380,120,-115) -> (636300,120,2525)
(984,288,-287) -> (969528,288,1393)
(1320,242,-240) -> (1163162,242,2158)
(2268,168,-162) -> (1472100,168,4206)
(3472,224,-217) -> (3017392,224,6503)
(5040,288,-280)
(5040,675,-672)
(7020,360,-351)
(9460,440,-430)
(11990,242,-218)
(12408,528,-517)
(15912,624,-612)
(28680,288,-239)

(28,8,-7) -> (21,1,7)
(132,24,-22) -> (110,2,22)
(168,49,-48) -> (120,1,48)
(360,48,-45) -> (65208,48,627)
(861,49,-41) -> (165648,49,1632)
(760,80,-76) -> (231880,80,1364)
(1380,120,-115) -> (636300,120,2525)
(984,288,-287) -> (969528,288,1393)
(1320,242,-240) -> (1163162,242,2158)
(2268,168,-162) -> (1472100,168,4206)
(3472,224,-217) -> (3017392,224,6503)
(5040,288,-280)
(5040,675,-672)
(7020,360,-351)
(9460,440,-430)
(11990,242,-218)
(12408,528,-517)
(15912,624,-612)
(28680,288,-239)


k^2 = m(2k+d)(m+1+d)

m = ax^2
k = axy
d = by

k^2 = m(2k+d)(m+1+d)
(axy)^2 = ax^2(2axy+by)(ax^2+1+by)
a^2x^2y^2 = ax^2(2ax+b)y(ax^2+1+by)
ay = (2ax+b)(ax^2+1+by)

ay = 2ax(ax^2+1+by) + b(ax^2+1+by)
ay = 2a^2x^3 + 2ax + 2abxy + abx^2 + b + b^2y


(2*2*7, 2*2^2, -7)
(2*6*11, 6*2^2, -2*11)
(7*1*24, 1*7^2, -2*24)
(2*12*15, 12*2^2, -3*15)
(7*3*41, 1*7^2, -41)
(4*5*38, 5*4^2, -2*38)
(2*30*23, 30*2^2, -5*23)
(12*2*41, 2*12^2, -7*41)
(11*2*60, 2*11^2, -4*60)
(2*42*27, 42*2^2, -6*27)
(2*56*31, 56*2^2, -7*31)


(3472,224,-217)

(24*41, 288, -7*41)
(24*41)^2 = 288*(2*24*41-


28^2 = 8*(2*28-7)*(8+1-7)
132^2 = 24*(2*132-22)*(24+1-22)
168^2 = 49*(2*168-48)*(49+1-48)

(360, 48, 45)
360^2 = 48*(2*360-45)*(48+1-45)
(8*45)^2 = 48*(2*8-1)*45*(48+1-45)

(4*7, 2*4, -7)
(12*11, 2*12, -2*11)

--------------------------

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

-- prune_tree z t = prune z (tree t)
prune_tree :: Integer -> Triple -> [Integer]
prune_tree z t@(k,_,_)
  | k > z = []
  | otherwise = k : S.union (prune_tree z (next t))
                            (prune_tree z (next (conv t)))

all_upto :: Integer -> [Integer]
all_upto z =
  foldr S.union []
    (takeWhile (not . null) [ prune_tree z (basic m) | m <- [1 ..] ])

{-
Case m=1:

k^2 = (2k+d)(d+2)
k^2 = 2k(d+2) + d(d+2)
k^2 = k(2d+4) + d(d+2)


(k,m,d) -> (k+m(4k+2d+1), m, d+2k-m)
(k,m,d) -> (k-m(2d+2m+1), m, d-2k+m(4d+4m+3))



m <= mmax
1 <= d <= m-1           (let d = m-1)
2 <= m+1-d <= m
2m <= e <= m^2          (e = 2m)  (e-d = m+1)
2m(m+1) <= e(e-d) < e^2 <= m^4
sqrt(2) m < sqrt(2m(m+1)) <= r = sqrt(e(e-d)) <= m^2
3m < (2 + sqrt(2))m < k = e + r <= 2m^2

3m + m(12m+2+1) <= k+m(4k+2d+1) = k'
6m + 12m^2 <= k'

6m*(2m+1)



k+m(4k+2d+1) <= kmax
k + 4km + 2dm + m <= kmax
k(1 + 4m) + 2dm + m <= kmax
k(1 + 4m) <= kmax - 2dm - m
k <= (kmax - 2dm - m)/(4m + 1)

k <= z
k^2 <= z^2
e + sqrt(e*(e-d)) <= z^2



(2k+d)m(m+1+d)

-}

ks_upto :: Integer -> [Integer]
ks_upto kmax = S.union ks1 ks2
  where
    chop = takeWhile (\(k,m,d) -> k <= kmax)
    fst3 (k,m,d) = k
    bs = chop (map basic [1 ..])
    cs = chop (map (next . conv . next . basic) [1 ..])
    bss = takeWhile (not . null) (iterate (chop . map next) bs)
    css = takeWhile (not . null) (iterate (chop . map next) cs)
    ks1 = foldl S.union [] (map (map fst3) bss)
    ks2 = foldl S.union [] (map (map fst3) css)

-- 231880 and 74872084 are duplicates
    
-- 238850524648437 WRONG!


main :: IO String
main = return $ show $ sum $ all_upto (10^10)

answer :: String
answer = "238890850232021"



{-
k^2 = (2k+d)m(m+1+d)
let e = m(m+1+d)
k^2 = (2k+d)e

k^2 = 2ke + de
k^2 - 2ke = de
k^2 - 2ke + e^2 = de + e^2
(k-e)^2
k^2 - 2ke - de = 0


(k-e)^2 = k^2 - 2e + e^2

[ -B +/- sqrt (B^2 - 4AC) ] / 2A
A = 1
B = -2e
C = -de

[ 2e +/- sqrt (4e^2 + 4de) ] / 2
[ e +/- sqrt (e^2 + de) ]


k^2 + Bk + C = 0

m = 8, d = -7, e = 16

16(16+7) = 18

28^2 = 2*28*2 + (-7)*2


(28,8,-7)
(132,24,-22)
(168,49,-48)
(360,48,-45)
(861,49,-41)
(760,80,-76)
(1380,120,-115)
(984,288,-287)
(1320,242,-240)
(2268,168,-162)
(3472,224,-217)

k = e +/- sqrt (e^2 + d^2e^2)


-}

deltas xs = zipWith subtract xs (tail xs)

