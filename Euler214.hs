module Euler214 where
import PrimeArray ( totient_array )
import Primes ( totient )
import Memoize
import Data.Array.Unboxed
import Data.Array.ST

{-
Problem 214
Totient Chains

25 October 2008

Let φ be Euler's totient function, i.e. for a natural number n, φ(n) is the
number of k, 1 <= k <= n, for which gcd(k,n) = 1.

By iterating φ, each positive integer generates a decreasing chain of numbers
ending in 1. E.g. if we start with 5 the sequence 5,4,2,1 is generated.

Here is a listing of all chains with length 4:
5,4,2,1
7,6,2,1
8,4,2,1
9,6,2,1
10,4,2,1
12,4,2,1
14,6,2,1
18,6,2,1

Only two of these chains start with a prime, their sum is 12.

What is the sum of all primes less than 40 million which generate a chain of
length 25?
-}

{-
For all n, phi(2n) <= n.

If 2^k+1 is prime, then length (totient_chain (2^k+1)) = k+2.

All other totient chains of the same length have higher start values.

Least possible start value for length l = 2^(l-2) + 1.

phi(2n) = phi(n), if n is odd
phi(2n) = 2 phi(n), if n is even
-}

prob214 :: Int -> Int -> [Integer]
prob214 m l =
  [ toInteger p |
    p <- ps,
    let ts = chain (p-1),
    length (p:ts) == l ]
  where
    -- generating totients takes about 80% of time
    a = totient_array m
    pmin = 2^(l-2) + 1
    ps = [ n | n <- [pmin, pmin+2 .. m], a!n == n-1 ]
    chain 1 = [1]
    chain n = n : chain (a!n)

totient_chain :: Integer -> [Integer]
totient_chain 1 = [1]
totient_chain n = n : totient_chain (totient n)

main :: IO String
main = return $ show $ sum $ prob214 (40*10^6) 25 
-- 1677366278943
-- length (prob214 (40*10^6) 25) = 51147
