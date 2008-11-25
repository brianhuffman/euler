module Euler050 where
import Primes

------------------------------------------------------------------------------
-- 50. Which prime, below one-million, can be written as the sum of the most consecutive primes?

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
-- 997651
