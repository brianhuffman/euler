module Euler223 where
import Primes

{-
Problem 223
26 December 2008

Let us call an integer sided triangle with sides a ≤ b ≤ c barely
acute if the sides satisfy

a^(2) + b^(2) = c^(2) + 1.

How many barely acute triangles are there with perimeter ≤ 25,000,000?
-}

{-

a,b,c mod 3: [0,1]
------------
a and b cannot both be multiples of 3.

a,b,c mod 4: [0,1]
------------
a and b cannot both be even.


0,1,4,4,1
0,1,4,4,1
1,2,0,0,2

0,1,4,1
0,1,4,1
1,2,5,2


a^2 + b^2 = c^2 + 1
a^2 - 1 = c^2 - b^2
(a+1)(a-1) = (c+b)(c-b)

-}

mult_pf xs [] = xs
mult_pf [] ys = ys
mult_pf xs@((p,i):xs') ys@((q,j):ys') =
  case compare p q of
    LT -> (p,i) : mult_pf xs' ys
    GT -> (q,j) : mult_pf xs ys'
    EQ -> (p, i+j) : mult_pf xs' ys'

prob223 m = 
  [ (a,s) |
    (a,pf) <- apfs,
    let a2 = a^2 - 1,
    let rs = list_divisors_of_pf pf,
    let rss = [ (r, a2 `div` r) | r <- rs ],
    (r,s) <- takeWhile (\(r,s) -> 2*a <= s - r) rss,
    even (s - r),
    -- let b = (s - r) `div` 2,
    -- let c = (s + r) `div` 2,
    a + s <= m
  ]
  where
    pfs1 = [ (prime_factorization n) | n <- [1 ..] ]
    pfs2 = zipWith mult_pf pfs1 (drop 2 pfs1)
    apfs = zip [2 .. m`div`3] pfs2

prob223' m =
  [ (a,b,c) |
    (a,s) <- prob223 m,
    let r = (a^2 - 1) `div` s,
    let b = (s - r) `div` 2,
    let c = (s + r) `div` 2 ]

-- 49114849
-- 12499999
-- 61614848
