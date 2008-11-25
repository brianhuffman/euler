module Euler136 where
import PrimeArray
import Data.Array.Unboxed

{-
Problem 136
Discover when the equation x^2 − y^2 − z^2 = n has a unique solution.
  (with x,y,z in arithmetic progression)

29 December 2006

The positive integers, x, y, and z, are consecutive terms of an arithmetic
progression. Given that n is a positive integer, the equation,
x^2 - y^2 - z^2 = n, has exactly one solution when n = 20:

13^2 - 10^2 - 7^2 = 20

In fact there are twenty-five values of n below one hundred for which the
equation has a unique solution.

How many values of n less than fifty million have exactly one solution?
-}

{-
------------------------------------------------------
Analysis:

This has a unique solution for n =
  4, 16
  p*4, p*16 for any prime p == 1 mod 4
  p, p*4, p*16 for any prime p == 3 mod 4

A solution for a value of n means:
Finding a factorization r*s = n such that
(1) (r+s) is a multiple of 4.
(2) r < 3s

The only values of n with a solution are:
* n == 0 (mod 16)
* n == 4 (mod 8)
* n == 3 (mod 4)

------------------------------------------------------
Part 1: Showing uniqueness of solutions
n = 4, 16, prime p == 3 (mod 4), 4p, 16p

For n = 4, the only factorizations are:
(1,4) (2,2) (4,1)
Of these, the only one satisfying rule (1) is (2,2).

For n = 16, the only factorizations are:
(1,16) (2,8) (4,4) (8,2) (16,1)
Of these, the only one satisfying rule (1) is (4,4).

For prime p == 3 (mod 4), the only factorizations are:
(r,s) = (1,p) or (p,1)
Both satisfy rule (1), but only (1,p) satisfies rule (2).

For odd prime p, the possible factorizations of n = 4p are:
(1,4p) (2,2p) (4,p) (p,4) (2p,2) (4p,1)
Of these, the only ones satisfying rule (1) are:
(2,2p) (2p,2)
Of these, only (2,2p) satisfies rule (2).

For odd prime p, the possible factorizations of n = 16p are:
(1,16p) (2,8p) (4,4p) (8,2p) (16,p) (p,16) (2p,8) (4p,4) (8p,2) (16p,1)
Of these, the only ones satisfying rule (1) are:
(4,4p) (4p,4)
Of these, only (4,4p) satisfies rule (2).

------------------------------------------------------
Part 2: Showing non-uniqueness of other solutions

Case 1: Multiples of 32.
  Let n = 32a
  Valid factorizations include both (4,8a) and (8,4a).

Case 2: Odd composite times 16.
  Let n = 16ab, with a <= b and both a and b odd.
  Valid factorizations include (4,4ab) and (4a,4b).

Case 3: Odd composite times 4.
  Let n = 4ab, with a <= b and both a and b odd.
  Valid factorizations include (2,2ab) and (2a,2b).

Case 4: Odd composite.
  Let n = ab, with a < b and ab == 3 (mod 4).
  This means that one factor is 1 (mod 4) and the other is 3 (mod 4).
  Valid factorizations include (1,ab) and (a,b).

-}

prob136 :: Int -> Int
prob136 m = 2 + x + 2*y + z
  where
    -- greatest k such that 2*k+1 < m
    m' = (m-2) `div` 2
    -- greatest k such that 4*(2*k+1) < m
    m4 = (m-5) `div` 8
    -- greatest k such that 16*(2*k+1) < m
    m16 = (m-17) `div` 32
    a = odd_prime_array m'
    x = length [ n | n <- [1,3 .. m'], a!n ]
    y = length [ n | n <- [1 .. m16], a!n ]
    z = length [ n | n <- [m16+1 .. m4], a!n ]

main :: IO String
main = return $ show $ prob136 (50*10^6)
-- 2544559
