module Euler132 where
import Primes

------------------------------------------------------------------------------
-- 132. Determining the first forty prime factors of a very large repunit.
{-
A number consisting entirely of ones is called a repunit. We shall define
R(k) to be a repunit of length k.

For example, R(10) = 1111111111 = 11 * 41 * 271* 9091, and the sum of these
prime factors is 9414.

Find the sum of the first forty prime factors of R(10^9).
-}

{-
Problem: Find prime factors of R(n) = (10^n - 1)/9

Special cases:
  R(n) is never a multiple of 2.
  3 divides R(n) iff 3 divides n.
  R(n) is never a multiple of 5.

Does prime p > 5 divide R(n)?

Let k = multiplicative order of 10 (mod p).
That is, k is the least value such that 10^k == 1 (mod p).
Then k divides (p-1), the order of the multiplicative group.

R(n) == 0 (mod p)
9*R(n) == 0 (mod p)
10^n - 1 == 0 (mod p)
10^n == 1 (mod p)
k | n
k | gcd n (p-1)
10^(gcd n (p-1)) == 1 (mod p)
-}

prime_divides_repunit :: Integer -> Integer -> Bool
prime_divides_repunit n p = expMod 10 (gcd n (p-1)) p == 1
-- unfortunately, expMod overflows type Int.

prime_repunit_divisors :: Integer -> [Integer]
prime_repunit_divisors n =
  [ p | p <- drop 3 primes, prime_divides_repunit n p ]

prob132 :: Integer -> Integer
prob132 n = sum $ take 40 $ prime_repunit_divisors n

main :: IO String
main = return $ show $ prob132 (10^9)
-- 843296

{-[11,17,41,73,101,137,251,257,271,353,401,449,641,751,1201,1409,1601,3541,4001,4801,5051,9091,10753,15361,16001,19841,21001,21401,24001,25601,27961,37501,40961,43201,60101,62501,69857,76001,76801,160001]-}

