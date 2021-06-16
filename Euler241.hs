module Euler241 where
import Data.Ratio
import Primes
import Permutation
import EulerLib
import SquareRoot

{---------------------------------------------------------------------
Problem 241
Perfection Quotients

18 April 2009

For a positive integer n, let σ(n) be the sum of all divisors of n, so
e.g. σ(6) = 1 + 2 + 3 + 6 = 12.

A perfect number, as you probably know, is a number with σ(n) = 2n.

Let us define the perfection quotient of a positive integer as p(n) =
σ(n) / n.

Find the sum of all positive integers n ≤ 10^(18) for which p(n) has
the form k + 1/2, where k is an integer.

---------------------------------------------------------------------}

perfection_quotient :: Integer -> Rational
perfection_quotient n = sum_of_divisors n % n

brute_force :: [Integer]
brute_force =
  [ n | n <- [2, 4 ..], denominator (perfection_quotient n) == 2 ]
-- [2,24,4320,26208,


{---------------------------------------------------------------------

p(n) is a multiplicative function.

For prime q, p(q^k)
= (1 + q^1 + q^2 + ... + q^k) / q^k
= 1 + 1/q + 1/q^2 + ... + 1/q^k
< q/(q-1)

p(2^k) = (2^(k+1) - 1) / 2^k

---------------------------------------------------------------------}

twos :: Integer -> Int
twos n | odd n = 0
       | otherwise = 1 + twos (n `div` 2)

brute_force2 :: [Integer]
brute_force2 =
  [ n |
    m <- [1,3..],
    let sm = sum_of_divisors m,
    let k = twos sm + 1,
    let n = m * 2^k,
    let sn = sm * (2^(k+1) - 1),
    let q = sn % n,
    denominator q == 2
  ]
-- [2,24,4320,4680,26208,20427264,8910720,91963648,17428320,10200236032,197064960,21857648640,57575890944,...

----------------------------------------------------------------------

log2 :: Integer -> Int
log2 1 = 0
log2 n = 1 + log2 (n `div` 2)

merge_pf :: [(Integer, Int)] -> [(Integer, Int)] -> [(Integer, Int)]
merge_pf ps [] = ps
merge_pf [] qs = qs
merge_pf ps@((p,i) : ps') qs@((q,j) : qs') =
  case compare p q of
    LT -> (p,i) : merge_pf ps' qs
    GT -> (q,j) : merge_pf ps qs'
    EQ -> (p,i+j) : merge_pf ps' qs'

search :: Integer -> [Integer]
search nmax =
  [ n |
    k <- [1 .. log2 nmax],
    let a = 2^k,
    let pf = prime_factorization (sum_of_divisors a),
    n <- go (k-1) a pf
  ]
  where
    go k a pf | a > nmax = []
    go 0 a pf = [a]
    go k a pf =
      [ n |
        ((p,e), pf1) <- remove1 pf,
        a `mod` p /= 0,  -- can only choose any given p once!
        let s = (p^(e+1)-1) `div` (p-1), -- sigma(p^e)
        let (s', i) = divN s 2,
        i <= k,
        let pf2 = prime_factorization s',
        let pf' = merge_pf pf1 pf2,
        n <- go (k-i) (a*p^e) pf'
      ]

-- misses 4320
-- misses 57629644800
-- misses 17116004505600
-- misses 301183421949935616

----------------------------------------------------------------------

-- numbers where all prime factors have exponent > 1
multis nmax = go nmax (takeWhile (<10^2) primes)
  where
    go z [] = [1]
    go z (p : ps)
      | p^2 > z = [1]
      | otherwise =
          [ n1 * n2 |
            e <- takeWhile (\e -> p^e <= z) [2 ..],
            let n1 = p^e,
            n2 <- go (z `div` n1) ps
          ]
          ++ go z ps

search2 :: Integer -> Integer -> [Integer]
search2 nmax mmax =
  [ n |
    a <- multis mmax,
    let pf1 = [ (p,-e) | (p,e) <- prime_factorization a ],
    let pf2 = prime_factorization (sum_of_divisors a),
    let pf = merge_pf pf1 pf2,
    all (\(p,e) -> p < 20 || e <= 1) pf,
    n <- go a pf
  ]
  where
    go a pf | a > nmax = []
    go a ((2,-1) : pf) | all ((>=0) . snd) pf = [a]
    go a pf =
      [ n |
        let ps = [ p | (p, e) <- pf, e > 0 ],
        let ps' = if any (> 20) ps then [last ps] else ps,
        p <- ps',
        let pf1 = merge_pf pf [(p,-1)],
{-
        ((p,e), pf1) <- remove1 pf,
        e == 1,
        all (\(p',e') -> p' < p || p' < 20 || e' <= 0) pf1,
-}
        a `mod` p /= 0,  -- can only choose any given p once!
        let pf2 = prime_factorization (p+1),
        let pf' = merge_pf pf1 pf2,
        n <- go (a*p) pf'
      ]



{---------------------------------------------------------------------

All odd primes appearing in the denominator must cancel.
For odd prime q, the numerator of p(q) = q+1 has all factors < q.
*Some prime must appear with an exponent greater than 1.*
(Except for case n = 2.)

----------------------------------------------------------------------
How big can the largest prime factor be?

Large prime q.

p(q) =  (1+q) / q

-- 2 goes into the numerator at least once.

To get a q in the numerator, we will need to
find some smaller prime r such that q | sigma(r^k).

With k at least 2, assume 1 + r + r^2 = q.

q ~ 9 digits, r^k ~ 9 digits.

(There are 78498 primes below 1 million.)
(There are 3401 primes below 10^(4.5) = 31622.)

The largest prime with an exponent > 1 must be below 31622.

----------------------------------------------------------------------

With n < 10^18, p(n) will probably not get larger than 13/2.

This means large factors (>>13) in the numerator must be canceled.

----------------------------------------------------------------------
Known solutions

2 = 2
24 = 2^3 3
4320 = 2^5 3^3 5
4680 = 2^3 3^2 5 13
26208 = 2^5 3^2 7 13
8910720 = 2^7 3^2 5 7 13 17
17428320 = 2^5 3^2 5 7^2 13 19
20427264 = 2^9 3^2 11 13 31
91963648 = 2^8 7 19 37 73
197064960 = 2^8 3 5 19 37 73
8583644160 = 2^10 3^2 5 7 13 23 89
10200236032 = 2^14 7 19 31 151
21857648640 = 2^14 3 5 19 31 151
57575890944 = 2^13 3^2 11 13 43 127
57629644800 = 2^11 3 5^2 7^2 13 19 31
206166804480 = 2^11 3^2 5 7 13^2 31 61
17116004505600 = 2^11 3^4 5^2 7^2 11 13 19 31
1416963251404800 = 2^15 3^3 5^2 11 17 31 43 257
15338300494970880 = 2^17 3^3 5 7 19^2 37 73 127
75462255348480000 = 2^11 3^4 5^4 7^3 11^2 13 19 71
88898072401645056 = 2^9 3^4 11^3 31^2 61 83 331
301183421949935616 = 2^20 3 7 13^2 31 61 127 337


4320: (7/2)
    (3 3 7) / (2 2 2 2 2)
  (2 2 2 5) / (3 3 3)
      (2 3) / (5)

17116004505600: (11/2)
(3 3 5 7 13) / (2 2 2 2 2 2 2 2 2 2 2)
     (11 11) / (3 3 3 3)
        (31) / (5 5)
      (3 19) / (7 7)
     (2 2 3) / (11)
       (2 7) / (13)
     (2 2 5) / (19)
 (2 2 2 2 2) / (31)

----------------------------------------------------------------------

σ(2^1) = 3
σ(2^2) = 7
σ(2^3) = 3 5
σ(2^4) = 31
σ(2^5) = 3 3 7
σ(2^6) = 127
σ(2^7) = 3 5 17
σ(2^8) = 7 73
σ(2^9) = 3 11 31
σ(2^10) = 23 89
σ(2^11) = 3 3 5 7 13
σ(2^12) = 8191
σ(2^13) = 3 43 127
σ(2^14) = 7 31 151
σ(2^15) = 3 5 17 257
σ(2^16) = 131071
σ(2^17) = 3 3 3 7 19 73
σ(2^18) = 524287
σ(2^19) = 3 5 5 11 31 41
σ(2^20) = 7 127 337
σ(2^21) = 3 23 89 683
σ(2^22) = 47 178481
σ(2^23) = 3 3 5 7 13 17 241
σ(2^24) = 31 601 1801
σ(2^25) = 3 2731 8191
σ(2^26) = 7 73 262657
σ(2^27) = 3 5 29 43 113 127
σ(2^28) = 233 1103 2089
σ(2^29) = 3 3 7 11 31 151 331
σ(2^30) = 2147483647

σ(3^1) = 2 2
σ(3^2) = 13
σ(3^3) = 2 2 2 5
σ(3^4) = 11 11
σ(3^5) = 2 2 7 13

σ(5^1) = 2 3
σ(5^2) = 31
σ(5^3) = 2 2 3 13
σ(5^4) = 11 71

σ(7^1) = 2 2 2
σ(7^2) = 3 19

σ(11) = 2 2 3
σ(13) = 2 7
σ(17) = 2 3 3
σ(19) = 2 2 5
σ(23) = 2 2 2 3
σ(31) = 2 2 2 2 2
σ(37) = 2 19
σ(43) = 2 2 11
σ(73) = 2 37
σ(89) = 2 3 3 5
σ(151) = 2 2 2 19
σ(127) = 2 2 2 2 2 2 2
σ(337) = 2 13 13

----------------------------------------------------------------------

Largest powers below 10^18:

2^59
3^37
5^25
7^21
11^17
13^16
17^14  19^14
23^13
.. 31^12
.. 43^11
.. 61^10
.. 97^9
.. 173^8
.. 367^7
.. 997^6
.. 3967^5
.. 31607^4




-----------------------------------------

Define t(n) = largest k such that 2^k | n.

Define s(n) = t(sigma(n)) - t(n)

We are looking for n such that s(n) = -1.

s(2^k) = -k

For odd n, s(n) is always positive.

For primes p == 1 (mod 4),
  s(p) >= 2.
  s(p^2) = 
  s(p^k) = t(k+1)  (is this right?)

For primes p == 3 (mod 4)


-}

-- s n = twos (sum_of_divisors n) - twos n

solutions :: [Integer]
solutions =
  [ 2
  , 24
  , 4320
  , 4680
  , 26208
  , 8910720
  , 17428320
  , 20427264
  , 91963648
  , 197064960
  , 8583644160
  , 10200236032
  , 21857648640
  , 57575890944
  , 57629644800
  , 206166804480
  , 17116004505600
  , 1416963251404800
  , 15338300494970880
  , 75462255348480000
  , 88898072401645056
  , 301183421949935616
  ]

main :: IO String
main = return $ show $ sum solutions

answer :: String
answer = "482316491800641154"
-- 16755568466430082 WRONG!
-- 16755626096074882 WRONG!
-- 317956164050516098 WRONG!
-- 406854236452161154 WRONG!

