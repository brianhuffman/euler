module Euler133 where
import Primes

------------------------------------------------------------------------------
-- 133. Investigating which primes will never divide a repunit containing 10^n digits.
{-
A number consisting entirely of ones is called a repunit. We shall define
R(k) to be a repunit of length k; for example, R(6) = 111111.

Let us consider repunits of the form R(10n).

Although R(10), R(100), or R(1000) are not divisible by 17, R(10000) is
divisible by 17. Yet there is no value of n for which R(10^n) will divide
by 19. In fact, it is remarkable that 11, 17, 41, and 73 are only four
primes below one-hundred that can ever be a factor of R(10^n).

Find the sum of all the primes below one-hundred thousand that will never
be a factor of R(10^n).
-}

{-
See also Problem 132.

For p > 5,
p | R(n) <--> 10^(gcd n (p-1)) == 1 (mod p)

exists n, p| R(10^n)
exists n, 10^(gcd (10^n) (p-1)) == 1 (mod p)
10^(2^a * 5^b) == 1 (mod p),
  where a = largest a, 2^a | p-1
        b = largest b, 5^b | p-1
-}

-- largest 2^a*5^b that divides n
twos_and_fives n = 2^a * 5^b
  where
    (m, a) = divN n 2
    (_, b) = divN m 5

never_factors :: [Integer]
never_factors = 2 : 3 : 5 :
  [ p | p <- drop 3 primes,
    expMod 10 (twos_and_fives (p-1)) p /= 1 ]

prob133 :: Integer -> Integer
prob133 m = sum $ takeWhile (< m) never_factors

main :: IO String
main = return $ show $ prob133 (10^5)
-- 453647705
