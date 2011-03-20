module Euler304 where
import Primes

{-

Problem 304
Primonacci
03 October 2010

For any positive integer n the function next_prime(n) returns the
smallest prime p such that p>n.

The sequence a(n) is defined by: a(1)=next_prime(10^14) and
a(n)=next_prime(a(n-1)) for n>1.

The fibonacci sequence f(n) is defined by: f(0)=0, f(1)=1 and
f(n)=f(n-1)+f(n-2) for n>1.

The sequence b(n) is defined as f(a(n)).

Find ∑b(n) for 1≤n≤100 000. Give your answer mod 1234567891011.

-}

primes_after n = filter miller_rabin candidates
  where
    n0 = n - n `mod` 30
    candidates = dropWhile (<n)
      [ m+k | m <- [n0, n0+30 ..], k <- [1,7,11,13,17,19,23,29] ]

-- a(n) ranges from 10^14 + 31 to 10^14 + 3235443

{-


-}

-- fibonacci n `mod` m
fibonacciMod m n = fst (f n)
  where
    f 0 = (0, 1)
    f n
      | even n = ((2*a*b - a^2)`mod`m, (a^2 + b^2)`mod`m)
      | otherwise = ((a^2 + b^2)`mod`m, (2*a*b + b^2)`mod`m)
      where (a, b) = f (n `div` 2)

prob304a modulus pmin nmax =
  sum (map (fibonacciMod modulus) (take nmax (primes_after pmin))) `mod` modulus

-- Faster version: Don't recompute fibonaccis from scratch each time

fibonacciMod_from m n = fs (f n)
  where
    f 0 = (0, 1)
    f n
      | even n = ((2*a*b - a^2)`mod`m, (a^2 + b^2)`mod`m)
      | otherwise = ((a^2 + b^2)`mod`m, (2*a*b + b^2)`mod`m)
      where (a, b) = f (n `div` 2)
    fs (a, b) = a : fs (b, if c < m then c else c-m)
      where c = a + b

every_other (x : y : zs) = x : every_other zs
every_other _ = []

sequence_b m n0 = [ f | (n, f) <- zip ns fs, miller_rabin n ]
  where
    n' = if even n0 then n0+1 else n0
    ns = [n', n'+2 ..]
    fs = every_other (fibonacciMod_from m n')

prob304b modulus pmin nmax =
  sum (take nmax (sequence_b modulus pmin)) `mod` modulus

main :: IO String
main = return $ show $ prob304b 1234567891011 (10^14) 100000

answer :: String
answer = "283988410192"
