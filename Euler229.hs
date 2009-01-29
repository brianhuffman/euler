module Euler229 where
import EulerLib (square_root)
import PrimeArray (primes_upto)
import Primes (invMod, prime_factorization)
import Data.Array.Unboxed

{-
Problem 229
Four Representations using Squares

24 January 2009

Consider the number 3600. It is very special, because

      3600 = 48^(2) +   36^(2)
      3600 = 20^(2) + 2×40^(2)
      3600 = 30^(2) + 3×30^(2)
      3600 = 45^(2) + 7×15^(2).

Similarly, we find that 88201 = 99^(2) + 280^(2) = 287^(2) + 2×54^(2)
= 283^(2) + 3×52^(2) = 197^(2) + 7×84^(2).

In 1747, Euler proved which numbers are representable as a sum of two
squares. We are interested in the numbers n which admit
representations of all of the following four types:

      n = a_(1)^(2) +   b_(1)^(2)
      n = a_(2)^(2) + 2 b_(2)^(2)
      n = a_(3)^(2) + 3 b_(3)^(2)
      n = a_(7)^(2) + 7 b_(7)^(2),

where the a_(k) and b_(k) are positive integers.

There are 75373 such numbers that do not exceed 10^(7).
How many such numbers are there that do not exceed 2×10^(9)?
-}


{-

If x = a^2 + b^2, and y = c^2 + d^2,
then xy = (ac - bd)^2 + (ad + bc)^2.

Let x = a^2 + kb^2
Let y = c^2 + kd^2
Then xy = (ac + kbd)^2 + k(ad - bc)^2
Also xy = (ac - kbd)^2 + k(ad + bc)^2

Therefore the set of such numbers is closed w.r.t. multiplication.

If zero is not allowed, then there may be exceptions.
This can only happen when ad=bc AND ac=kbd.
THIS can only happen when c^2 = kd^2, and k is square.
Specifically, 2 = 1^2 + 1^2, but 4 is not the sum of positive squares.

There are no exceptions for k = 2, 3, or 7.

----------------------------

Let x = a^2 + kb^2
Then xn^2 = (an)^2 + k(bn)^2

Therefore the set of such numbers is closed w.r.t. scaling by squares.

----------------------------

For squarefree k, kn has the form a^2 + kb^2 iff n does.
Proof: Let kn = a^2 + kb^2.
       Then k divides a^2.
       Then k divides a; let a = kc.
       Then n = b^2 + kc^2.

-----------------------------------------
-- Numbers of the form a^2 + b^2:

An odd prime p has the form a^2 + b^2 iff p == 1 (mod 4).

2^n has the form a^2 + b^2 iff n is odd.

Number n has the form a^2 + b^2 iff
  1) all prime divisors == 3 (mod 4) have even exponents, and
  2) n is an odd power of 2, or there exists a prime divisor == 1 (mod 4).

-----------------------------------------
-- Numbers of the form a^2 + 2b^2:

Prime p has the form a^2 + 2b^2 iff p == 1 or 3 (mod 8).

2n has the form a^2 + 2b^2 iff n does.

Number n has the form a^2 + 2b^2 iff
  1) all prime divisors == 5 or 7 (mod 8) have even exponents, and
  2) there exists a prime divisor == 1 or 3 (mod 8).

-----------------------------------------
-- Numbers of the form a^2 + 3b^2:

Prime p has the form a^2 + 2b^2 iff p == 1 (mod 6).

3n has the form a^2 + 3b^2 iff n does.

4n has the form a^2 + 3b^2 iff n does (for non-square n).
Proof: 4n = a^2 + 3b^2.
  Case 1: a and b are both even; let a = 2c and b = 2d.
     Then n = c^2 + 3d^2.
  Case 2: a and b both odd, a == b (mod 4).
     Then 4n = a^2 + 3b^2.
     Then n = ((a+3b)/4)^2 + 3((a-b)/4)^2.
     (Except in case where a = b).
  Case 3: a and b both odd, a=4c+3, b=4d+1.
     Then 4n = (4c+3)^2 + 3(4d+1)^2.
     Then n = (3d-c)^2 + 3(c+d+1)^2.
  Case 4: a and b both odd, a=4c+1, b=4d+3.
     Then 4n = (4c+1)^2 + 3(4d+3)^2.
     Then n = (3d-c+2)^2 + 3(c+d+1)^2.
Qed.

Number n has the form a^2 + 3b^2 iff
  1) all prime divisors == 5 (mod 6) have even exponents, and
  2) divisor 2 has an even exponent, and
  3) 4 divides n, or there exists a prime divisor == 1 (mod 6).

-----------------------------------------
-- Numbers of the form a^2 + 7b^2:

Prime p has the form a^2 + 7b^2 iff p == 1, 9, 11 (mod 14).

7n has the form a^2 + 7b^2 iff n does.

2^n has the form a^2 + 7b^2 iff n > 2.

Number n has the form a^2 + 7b^2 iff
  1) all prime divisors == 3,5,13 (mod 14) have even exponents, and
  2) n /= 2 (mod 4)
  3) 8 divides n, or there exists a prime divisor == 1,9,11 (mod 14).

-----------------------------------------
-- Numbers of all 4 forms:

Prime p is a "good" prime (has all 4 forms) iff:
  1) p == 1 (mod 4), and
  2) p == 1 or 3 (mod 8), and
  3) p == 1 (mod 6), and
  4) p == 1 or 9 or 11 (mod 14).

Prime p is "good" iff p == 1, 25, or 121 (mod 168).

All other primes are "bad" primes.

Number n has all 4 forms iff:
  1) all "bad" prime divisors have even exponents, and
  2) there exists a prime divisor == 1 (mod 4).
  3) there exists a prime divisor == 1 or 3 (mod 8).
  4) 4 divides n, or there exists a prime divisor == 1 (mod 6).
  5) 8 divides n, or there exists a prime divisor == 1,9,11 (mod 14).

-}

-- good_square n = can n^2 be written in all 4 forms
good_square :: Int -> Bool
good_square n = ok1 && ok2 && ok3 && ok7
  where
    ps = map fst (prime_factorization n)
    ok1 = any (\p -> p`mod`4 == 1) ps
    ok2 = any (\p -> (p`mod`8)`elem`[1,3]) ps
    ok3 = even n || any (\p -> p`mod`6 == 1) ps
    ok7 = n`mod`4 == 0 || any (\p -> (p`mod`14)`elem`[1,9,11]) ps

good_primes :: Int -> [Int]
good_primes m = good_ps
  where
    -- omit 2, 3, and 7 (they are not coprime to 168)
    small_ps = 5 : drop 4 (primes_upto (square_root m))
    imax = m `div` 168
    a1 = arr 1
    a2 = arr 25
    a3 = arr 121
    good_ps =
      [ 168*i + k |
        i <- [1 .. imax],
        (a,k) <- [(a1,1),(a2,25),(a3,121)],
        a!i ]
    -- arr k ! i = is_prime (168*i + k)
    arr :: Int -> UArray Int Bool
    arr k = accumArray (const id) True (0,imax)
      [ (i, False) |
        p <- small_ps,
        -- p divides (168*i0 + k)
        let i0 = if p`mod`168 == k then p`div`168 + p
                 else (-k * invMod 168 p) `mod` p,
        i <- [i0, i0+p .. imax] ]

products_upto :: Int -> [Int] -> [Int]
products_upto m xs = prods 1 xs []
  where
    prods a [] ts = a : ts
    prods a (x:xs) ts
      | x <= m`div`a = prods (a*x) xs (prods a xs ts)
      | otherwise = a : ts

prob229 :: Int -> Int
prob229 m =
  length (filter good_square [1 .. square_root m]) +
  sum [ square_root (m `div` n) | n <- init (products_upto m (good_primes m)) ]

main :: IO String
main = return $ show $ prob229 (2*10^9)

answer :: String
answer = "11325263"
