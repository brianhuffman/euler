{-# LANGUAGE BangPatterns #-}

module Euler276 where
import Primes (prime_factorization)

{---------------------------------------------------------------------
Problem 276
Primitive Triangles

29 January 2010

Consider the triangles with integer sides a, b and c with a ≤ b ≤ c.
An integer sided triangle (a,b,c) is called primitive if gcd(a,b,c)=1.
How many primitive integer sided triangles exist with a perimeter not
exceeding 10 000 000?

---------------------------------------------------------------------}

-- 3-way gcd algorithm for triangles:
-- invariant: a <= b <= c <= a+b
gcd3 a b c
  | a == 0    = c
  | x < a     = gcd3 x a b
  | otherwise = gcd3 a x b
  where
    x = c-a

gcd3' a b c
  | a == 0    = []
  | x < a     = (a,b,c) : gcd3' x a b
  | otherwise = (a,b,c) : gcd3' a x b
  where
    x = c-a

{---------------------------------------------------------------------

Running gcd3 in reverse can generate all valid triangles.

a <= b <= c < a+b

---------------------------------------------------------------------}

type Z = Int

triangles :: Z -> [(Z, Z, Z)]
triangles tmax = filter ok (go 1 1 1 3)
  where
    ok (a,b,c) = c < a + b
    go :: Z -> Z -> Z -> Z -> [(Z, Z, Z)]
    go !a !b !c !t
      | t > tmax  = []
      | a == b    = (a,b,c) : go a c (a+b) (t+a)
      | otherwise = (a,b,c) : go a c (a+b) (t+a) ++ go b c (a+b) (t+b)

-- num_triangles = length (triangles tmax)
num_triangles :: Z -> Integer
num_triangles tmax = go 1 1 1 3
  where
    go :: Z -> Z -> Z -> Z -> Integer
    go !a !b !c !t
      | t > tmax  = 0
      | a == b    = k + go a c (a+b) (t+a)
      | otherwise = k + go a c (a+b) (t+a) + go b c (a+b) (t+b)
      where k = if a + b > c then 1 else 0

{-
num_triangles 10 = 8
num_triangles 30 = 79
num_triangles 100 = 6033
num_triangles 300 = 158300
num_triangles 1000 = 5803431
num_triangles 3000 = 156214655

Run-time is proportional to result, which is cubic.
This will not work for 10^7.
-}

brute_force :: Z -> [(Z, Z, Z)]
brute_force tmax =
  [ (a, b, c) |
    a <- [1 .. tmax `div` 3],
    b <- [a .. (tmax-a) `div` 2],
    let d = gcd a b,
    c <- [b .. min (tmax-a-b) (a+b-1)],
    gcd c d == 1
  ]

{---------------------------------------------------------------------

Speeding up the brute force algorithm:

We need a fast way to count how many of [a..b] are coprime to n.

---------------------------------------------------------------------}

-- (products of even-length sublists, products of odd-length sublists)
even_odd_products :: [Z] -> ([Z], [Z])
even_odd_products [] = ([1], [])
even_odd_products (x : xs) = (ys', zs')
  where
    (ys, zs) = even_odd_products xs
    ys' = map (*x) zs ++ ys
    zs' = map (*x) ys ++ zs

-- num_coprime_upto m n = length [ x | x <- [1..m], gcd x n == 1 ]
num_coprime_upto m n = sum_div ys - sum_div zs
  where
    ps = map fst (prime_factorization n)
    (ys, zs) = even_odd_products ps
    sum_div xs = sum (map (m `div`) xs)

-- num_coprime_between a b n = length [ x | x <- [a..b], gcd x n == 1 ]
num_coprime_between a b n =
  num_coprime_upto b n - num_coprime_upto (a-1) n

num_triangles2 :: Z -> Integer
num_triangles2 tmax = sum
  [ fromIntegral len |
    a <- [1 .. tmax `div` 3],
    b <- [a .. (tmax-a) `div` 2],
    let d = gcd a b,
    let cmax = min (tmax-a-b) (a+b-1),
    let len = num_coprime_between b cmax d
  ]

{-
triangle numbers T(n) = (SUM [1..n]. n) = (x^2 + x) / 2
pyramid numbers P(n) = (SUM [1..n]. T(n)) = (x^3 + 3x^2 + 2x) / 6



num_triangles2 100 = 6033
num_triangles2 1000 = 5803431
num_triangles2 10000 = 5779731138 (15 s)
num_triangles2 20000 = 46227805268 (60 s)

Run-time is still quadratic.
This will not work for 10^7.
-}

pyramid :: Z -> Z
pyramid n = (n^3 + 3*n^2 + 2*n) `div` 6

all_triangles :: Z -> [(Z,Z,Z)]
all_triangles tmax =
  [ (a,b,c) |
    a <- [1 .. tmax `div` 3],
    b <- [a .. (tmax-a) `div` 2],
    c <- [b .. min (tmax-a-b) (a+b-1)]
  ]

num_all tmax = sum (map (const 1) (all_triangles tmax))

all_triangles' :: Z -> [(Z,Z,Z)]
all_triangles' tmax =
  [ (a,b,c) |
    a <- [1 .. tmax `div` 3],
    b <- [1 .. (tmax-a) `div` 2],
    let cmin = abs (a - b) + 1,
    let cmax = (a + b) - 1,
    c <- [cmin .. min (tmax-a-b) cmax]
  ]

deltas xs = zipWith subtract xs (tail xs)



{-
length (all_triangles (6*n  )) = 3/2 n^3 + 3/2 n^2
length (all_triangles (6*n+3)) = 3/2 n^3 + 15/4 n^2 + 3 n + ...


[3,18,54,120,225,378,588,864,1215,1650,2178,2808,3549,4410,5400,6528]
-}
