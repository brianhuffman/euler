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

primitives_slow :: Z -> [(Z, Z, Z)]
primitives_slow tmax =
  [ (a, b, c) |
    a <- [1 .. tmax `div` 3],
    b <- [a .. (tmax-a) `div` 2],
    let d = gcd a b,
    c <- [b .. min (tmax-a-b) (a+b-1)],
    gcd c d == 1
  ]

-- 3-way gcd algorithm for triangles:
-- invariant: a <= b <= c <= a+b
gcd3 a b c
  | a == 0    = c
  | x < a     = gcd3 x a b
  | otherwise = gcd3 a x b
  where
    x = c-a

{---------------------------------------------------------------------

Running gcd3 in reverse can generate all valid triangles.

a <= b <= c < a+b

---------------------------------------------------------------------}

type Z = Integer

primitive_triangles :: Z -> [(Z, Z, Z)]
primitive_triangles tmax = filter ok (go 1 1 1 3)
  where
    ok (a,b,c) = c < a + b
    go :: Z -> Z -> Z -> Z -> [(Z, Z, Z)]
    go !a !b !c !t
      | t > tmax  = []
      | a == b    = (a,b,c) : go a c (a+b) (t+a)
      | otherwise = (a,b,c) : go a c (a+b) (t+a) ++ go b c (a+b) (t+b)

-- prob276a = length (primitive_triangles tmax)
prob276a :: Z -> Integer
prob276a tmax = go 1 1 1 3
  where
    go :: Z -> Z -> Z -> Z -> Integer
    go !a !b !c !t
      | t > tmax  = 0
      | a == b    = k + go a c (a+b) (t+a)
      | otherwise = k + go a c (a+b) (t+a) + go b c (a+b) (t+b)
      where k = if a + b > c then 1 else 0

{-
prob276a 10 = 8
prob276a 30 = 79
prob276a 100 = 6033
prob276a 300 = 158300
prob276a 1000 = 5803431
prob276a 3000 = 156214655

Run-time is proportional to result, which is cubic.
This will not work for 10^7.
-}

{-
Alternative approach: count all triangles up to maximum perimeter,
primitive or not.
-}

all_triangles :: Z -> [(Z,Z,Z)]
all_triangles tmax =
  [ (a,b,c) |
    a <- [1 .. tmax `div` 3],
    b <- [a .. (tmax-a) `div` 2],
    c <- [b .. min (tmax-a-b) (a+b-1)]
  ]

-- num_triangles tmax = length (all_triangles tmax)
-- polynomials found by experiment
num_triangles tmax
  | r == 0 = 12*n^3 + 6*n^2
  | r == 1 = 12*n^3 + 9*n^2 + 2*n
  | r == 2 = 12*n^3 + 12*n^2 + 3*n
  | r == 3 = 12*n^3 + 15*n^2 + 6*n + 1
  | r == 4 = 12*n^3 + 18*n^2 + 8*n + 1
  | r == 5 = 12*n^3 + 21*n^2 + 12*n + 2
  | r == 6 = 12*n^3 + 24*n^2 + 15*n + 3
  | r == 7 = 12*n^3 + 27*n^2 + 20*n + 5
  | r == 8 = 12*n^3 + 30*n^2 + 24*n + 6
  | r == 9 = 12*n^3 + 33*n^2 + 30*n + 9
  | r == 10 = 12*n^3 + 36*n^2 + 35*n + 11
  | r == 11 = 12*n^3 + 39*n^2 + 42*n + 15
  where
    (n, r) = divMod tmax 12

{-
inclusion/exclusion algorithm.
num_triangles (tmax `div` d) counts...
 * positive if d is a product of odd number of distinct primes
 * negative if d is a product of even number of distinct primes
 * zero otherwise
-}

prob276b tmax = sum [ do_divisor d | d <- [1..tmax`div`3] ]
  where
    count d [] = num_triangles (tmax`div`d)
    count d ((p,1):pf) = - count d pf
    count d ((p,e):pf) = 0
    do_divisor d = count d (prime_factorization d)

-- prob276b (10^4) = 5779731138
-- prob276b (10^5) = 5777395503473
-- prob276b (10^6) = 5777160265590041

main :: IO String
main = return $ show $ prob276b (10^7)

answer :: String
answer = "5777137137739632912"
