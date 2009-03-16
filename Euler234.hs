module Euler234 where
import Primes

{-
Problem 234
Semidivisible numbers

28 February 2009

For an integer n ≥ 4, we define the lower prime square root of n,
denoted by lps(n), as the largest prime ≤ √n and the upper prime
square root of n, ups(n), as the smallest prime ≥ √n.

So, for example, lps(4) = 2 = ups(4), lps(1000) = 31, ups(1000) = 37.
Let us call an integer n ≥ 4 semidivisible, if one of lps(n) and
ups(n) divides n, but not both.

The sum of the semidivisible numbers not exceeding 15 is 30, the
numbers are 8, 10 and 12.  15 is not semidivisible because it is a
multiple of both lps(15) = 3 and ups(15) = 5.  As a further example,
the sum of the 92 semidivisible numbers up to 1000 is 34825.

What is the sum of all semidivisible numbers not exceeding 999966663333 ?

-}

{-
-- 2^2 = 4
2*3 = 6 is ruled out.
2*4 = 8
-- 3^2 = 9
2*5 = 10
3*4 = 12
3*5 = 15 is ruled out.
4*5 = 20
-- 5^2 = 25
5*6 = 30
-- 7^2 = 49
7*8 = 56
7*9 = 63
7*10 = 70
11*5 = 55
11*6 = 66
11*8 = 88
11*9 = 99
11*10 = 110
-- 11^2 = 121
-- 13^2 = 169
-- 17^2 = 289

-}


semidivisibles (p, q) = concat [ps1, qs1, ps2, qs2]
  where
    ps1 = [ p*k | k <- [p+1 .. q-1] ]
    qs1 = [ q*k | k <- [p+1 .. q-1] ]
    ps2 = takeWhile (< q^2) [ p*k | k <- [q+1, q+2 ..] ]
    qs2 = takeWhile (> p^2) [ q*k | k <- [p-1, p-2 ..] ]

semidivisibles_upto m = concatMap f pqs
  where
    pairs = zip primes (tail primes)
    pqs = takeWhile (\(p,q) -> p^2 <= m) pairs
    f pq = filter (<= m) (semidivisibles pq)

main :: IO String
main = return $ show $ sum $ semidivisibles_upto 999966663333

answer :: String
answer = "1259187438574927161"
