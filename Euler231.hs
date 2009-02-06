module Euler231 where
import PrimeArray

{-
Problem 231
06 February 2009

The binomial coefficient ^(10)C_(3) = 120.

120 = 2^(3) × 3 × 5 = 2 × 2 × 2 × 3 × 5, and 2 + 2 + 2 + 3 + 5 = 14.

So the sum of the terms in the prime factorisation of ^(10)C_(3) is
14.

Find the sum of the terms in the prime factorisation of
^(20000000)C_(15000000).

-}

{-
choose 10 3 = 120

choose m n = factorial m / factorial n / factorial (m-n)
-}

exponent_in_factorial :: Int -> Int -> Int
exponent_in_factorial p n =
  if n == 0 then 0 else
  let m = n `div` p in m + exponent_in_factorial p m

exponent_in_choose :: Int -> Int -> Int -> Int
exponent_in_choose p m n =
  exponent_in_factorial p m
  - exponent_in_factorial p n
  - exponent_in_factorial p (m - n)

prob231 :: Int -> Int -> Integer
prob231 m n = sum [ toInteger (p * exponent_in_choose p m n) | p <- ps ]
  where ps = primes_upto m

main :: IO String
main = return $ show $ prob231 (20*10^6) (15*10^6)

answer :: String
answer = "7526965179680"
