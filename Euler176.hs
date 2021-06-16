module Euler176 where
import Primes
import Data.List

{-
Problem 176
Rectangular triangles that share a cathetus.

04 January 2008

The four rectangular triangles with sides (9,12,15), (12,16,20), (5,12,13)
and (12,35,37) all have one of the shorter sides (catheti) equal to 12. It
can be shown that no other integer sided rectangular triangle exists with
one of the catheti equal to 12.

Find the smallest integer that can be the length of a cathetus of exactly
47547 different integer sided rectangular triangles.
-}

{-
a^2 + b^2 = c^2
a^2 = c^2 - b^2
a^2 = (c + b)(c - b)
if a is odd, then all x, y such that x*y = a^2 will work.
if a is even, then only even x, y such that x*y = a^2 will work.

95095 = 5*7*11*13*19
[2,3,5,6,9]

2^9 * 3^6 * 5^5 * 7^3 * 11^2
-}

-- list all possible b for pythagorean triples (a,b,c)
other_catheti a = takeWhile (>0)
  [ b2 `div` 2 |
    (m,n) <- zip fs (reverse fs),
    let b2 = n - m,
    even b2 ]
  where
    pf2 = [ (p, 2*e) | (p,e) <- prime_factorization a ]
    fs = list_divisors_of_pf pf2

-- num_catheti a == length (other_catheti a)
num_catheti a = product [ 2*e+1 | (p,e) <- pf ] `div` 2
  where
    a' = if even a then a `div` 2 else a
    pf = prime_factorization a'

-- prob176 n = least a such that num_catheti a = n
prob176 n = 2 * product rs
  where
    m = 2*n + 1
    pf = prime_factorization m
    ps = concatMap (\(p,e) -> genericReplicate e p) pf
    qs = reverse $ sort $ map (`div`2) ps
    rs = zipWith (^) primes qs

main :: IO String
main = return $ show $ prob176 47547
-- 96818198400000
