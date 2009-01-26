module Euler223 where
import Data.Int (Int64)
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
a^2 + b^2 = c^2 + 1
a^2 - 1 = c^2 - b^2
(a+1)(a-1) = (c+b)(c-b)

Let r = c-b, s = c+b.

WLOG assume a <= b.
Then 2*a <= s - r.

For any n > 0, (1, n, n) is a solution.
It has perimeter 2n + 1.

For any n > 1, (2n, 2n^2-1, 2n^2) is a solution.
It has perimeter 4n^2 + 2n - 1.

For any n > 1, (2n+1, n^2+n-1, n^2+n+1) is a solution.
It has perimeter 2n^2 + 4n + 1.

-}

{-
a^2 + b^2 = c^2 + 1

If (a,b,c) is a solution, then (a',b',c') is also a solution:

a' = 2a +  b + 2c
b' =  a + 2b + 2c
c' = 2a + 2b + 3c

The transformation matrix has determinant 1, so it has an inverse with
integer coefficients:

|2 1 2|   | 2  1 -2|   |1 0 0|
|1 2 2| * | 1  2 -2| = |0 1 0|
|2 2 3|   |-2 -2  3|   |0 0 1|

(a - b) is an invariant of this transformation.

next (a,b,c) = (2a + b + 2c, a + 2b + 2c, 2a + 2b + 3c)
prev (a,b,c) = (2a + b - 2c, a + 2b - 2c, 3c - 2a - 2b)

If (a,b,c) is a solution with a,b,c all positive, then we have the
inequalities a, b <= c <= a + b.

c <= a + b
2c <= 2a + 2b
3c - 2a - 2b <= c

a <= c, b <= c
a + b <= 2c
2a + 2b <= 4c
-c <= 3c - 2a - 2b

We can conclude that |3c - 2a - 2b| <= c. The inequality is strict
unless a = b = c or a + b = c. This only happens for (1,1,1).

This means that prev always gives back a "smaller" solution. We can
use this fact to generate all solutions from a few base cases.

The only positive integer solutions to |3c - 2a - 2b| = 0 are (1,2,2)
and (2,1,2).

0 <= 3c - 2a - 2b
2a + 2b <= 3c
(2a + 2b)^2 <= (3c)^2
4a^2 + 4b^2 + 8ab <= 9c^2
4a^2 + 4b^2 + 8ab <= 9(c^2 + 1) - 9
4a^2 + 4b^2 + 8ab <= 9(a^2 + b^2) - 9
4a^2 + 4b^2 + 8ab <= 9a^2 + 9b^2 - 9
8ab <= 5a^2 + 5b^2
9 <= a^2 + b^2 + (4a^2 - 8ab + 4b^2)
9 <= a^2 + b^2 + 4(b - a)^2
This holds for all positive a and b, except when (a,b) = (1,1).
The inequality is strict except when (a,b) = (1,2) or (2,1).

Assuming that a <= b:
0 < a + 2b - 2c
2c < a + 2b
(2c)^2 < (a + 2b)^2
4c^2 < a^2 + 4ab + 4b^2
4(c^2 + 1) - 4 < a^2 + 4ab + 4b^2
4(a^2 + b^2) - 4 < a^2 + 4ab + 4b^2
4a^2 + 4b^2 - 4 < a^2 + 4ab + 4b^2
3a^2 - 4 < 4ab
3a^2 < 4ab + 4
This always holds if a <= b.

2a + b - 2c <= a + 2b - 2c
2a + b <= a + 2b
a <= b

Thus, for a positive solution (a,b,c) with 0 < a <= b, prev(a,b,c) =
(a',b',c') satisfies c' <= c, a' <= b', and 0 < b'.

Therefore, there are 3 remaining cases for how each canonical triple
can be generated from a previous canonical triple.

1: (a,b,c) -> next(a,b,c)
2: (a,b,c) -> next(-a,b,c)
3: (a,b,c) -> next(-b,a,c)

-}

{-
type Z = Int64

mult_pf xs [] = xs
mult_pf [] ys = ys
mult_pf xs@((p,i):xs') ys@((q,j):ys') =
  case compare p q of
    LT -> (p,i) : mult_pf xs' ys
    GT -> (q,j) : mult_pf xs ys'
    EQ -> (p, i+j) : mult_pf xs' ys'

barely_acute :: Z -> [(Z, Z)]
barely_acute m = 
  [ (a, s) |
    (a, pf) <- apfs,
    let a2 = a^2 - 1,
    let rs = list_divisors_of_pf pf,
    let rss = [ (r, a2 `div` r) | r <- rs ],
    let rss' = dropWhile (\(r, s) -> a + s > m) rss,
    (r, s) <- takeWhile (\(r, s) -> 2*a <= s - r) rss',
    even (s - r)
    -- let b = (s - r) `div` 2,
    -- let c = (s + r) `div` 2,
  ]
  where
    pfs1 = [ prime_factorization n | n <- [1 ..] ]
    pfs2 = zipWith mult_pf pfs1 (drop 2 pfs1)
    apfs = zip [2 .. m`div`3] pfs2

barely_acute_triangles :: Z -> [(Z, Z, Z)]
barely_acute_triangles m =
  [ (1, n, n) | n <- [1 .. (m-1) `div` 2] ] ++
  [ (a, b, c) |
    (a, s) <- barely_acute m,
    let r = (a^2 - 1) `div` s,
    let b = (s - r) `div` 2,
    let c = (s + r) `div` 2 ]
-}

barely_acute_triangles :: Int -> [(Int, Int, Int)]
barely_acute_triangles m = f [(1,1,1), (1,2,2)]
  where
    f [] = []
    f ((a,b,c):ts)
      | a + b + c > m = f ts
      | a == b = (a,b,c) : f (next(a,b,c) : next(-a,b,c) : ts)
      | otherwise = (a,b,c) : f (next(a,b,c) : next(-a,b,c) : next(-b,a,c) : ts)

next :: (Int, Int, Int) -> (Int, Int, Int)
next (a, b, c) = (2*a + b + 2*c, a + 2*b + 2*c, 2*a + 2*b + 3*c)

prev :: (Int, Int, Int) -> (Int, Int, Int)
prev (a, b, c) = (2*a + b - 2*c, a + 2*b - 2*c, 3*c - 2*a - 2*b)

-- prob223 m = length (barely_acute_triangles m)
prob223 :: Int -> Int
prob223 m = f [(1,1,1), (1,2,2)] 0
  where
    f [] n = n
    f ((a,b,c):ts) n
      | a + b + c > m = f ts n
      | a == b = f (next(a,b,c) : next(-a,b,c) : ts) $! (n+1)
      | otherwise = f (next(a,b,c) : next(-a,b,c) : next(-b,a,c) : ts) $! (n+1)

main :: IO String
main = return $ show $ prob223 (25*10^6)

answer :: String
answer = "61614848"
