module Euler274 where
import Primes (invMod, primes)

{---------------------------------------------------------------------
Problem 274
Divisibility Multipliers

15 January 2010

For each integer p > 1 coprime to 10 there is a positive divisibility
multiplier m < p which preserves divisibility by p for the following
function on any positive integer, n:

f(n) = (all but the last digit of n) + (the last digit of n) * m

That is, if m is the divisibility multiplier for p, then f(n) is
divisible by p if and only if n is divisible by p.

(When n is much larger than p, f(n) will be less than n and repeated
application of f provides a multiplicative divisibility test for p.)

For example, the divisibility multiplier for 113 is 34.

f(76275) = 7627 + 5 * 34 = 7797 : 76275 and 7797 are both divisible by
113

f(12345) = 1234 + 5 * 34 = 1404 : 12345 and 1404 are both not
divisible by 113

The sum of the divisibility multipliers for the primes that are
coprime to 10 and less than 1000 is 39517. What is the sum of the
divisibility multipliers for the primes that are coprime to 10 and
less than 10^(7)?

---------------------------------------------------------------------}

{---------------------------------------------------------------------

n = 10a + b
f(n) = a + mb

p | f(n) <--> p | n
<-->
p | a + mb <--> p | 10a + b

Choose m such that 10m == 1 (mod p).

p | a + mb
<-->
p | 10a + 10mb
<--> 
p | 10a + b

---------------------------------------------------------------------}

multiplier :: Int -> Int
multiplier = invMod 10

sum_multipliers :: Int -> Integer
sum_multipliers pmax = sum (map (fromIntegral . multiplier) ps)
  where ps = 3 : drop 3 (takeWhile (<pmax) primes)

main :: IO String
main = return $ show $ sum_multipliers (10^7)

answer :: String
answer = "1601912348822"
