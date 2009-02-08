module Euler050 where
import Primes

{-
Problem 50
15 August 2003

The prime 41, can be written as the sum of six consecutive primes:
41 = 2 + 3 + 5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime
below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to
a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most
consecutive primes?
-}

-- sum of at least 20 primes, max ~ 50000

prime_sums :: [Int]
prime_sums = scanl (+) 0 primes

-- sums of sequences of l consecutive primes
prime_seq_sums :: Int -> [Int]
prime_seq_sums l = zipWith subtract prime_sums (drop l prime_sums)

-- list of (prime sum, sequence length)
-- reverse sorted by l, then sorted by p
prime_sum_lengths :: Int -> [(Int, Int)]
prime_sum_lengths n =
  [ (p, l) |
    l <- [lmax, lmax-1 .. 1],
    p <- takeWhile (< n) (prime_seq_sums l),
    is_prime p ]
  where
    lmax = length $ takeWhile (< n) prime_sums

prob50 :: Int -> Int
prob50 n = fst $ head $ prime_sum_lengths n

main :: IO String
main = return $ show $ prob50 (10^6)

answer :: String
answer = "997651"
