module Euler047 where
import Primes
import List

{-
Problem 47
04 July 2003

The first two consecutive numbers to have two distinct prime factors are:

14 = 2 7
15 = 3 5

The first three consecutive numbers to have three distinct prime factors are:

644 = 2 2 7 23
645 = 3 5 43
646 = 2 17 19.

Find the first four consecutive integers to have four distinct primes factors.
What is the first of these numbers?
-}

count_prime_factors :: Int -> Int
count_prime_factors = length . prime_factorization

-- k <= count_prime_factors n
distinct_prime_factors :: Int -> Int -> Bool
distinct_prime_factors k n = foo primes n k
  where
    foo (p:ps) x k
      | k == 1    = x > 1
      | x < p^k   = False
      | otherwise =
          case divN x p of
            (_, 0) -> foo ps x k
            (y, e) -> foo ps y (k-1)

-- first l consecutive numbers with k prime factors
prob47 :: Int -> Int -> [Int]
prob47 k l =
  map fst $ head $
  filter (all snd) $
  map (take l) $ tails $
  [ (n, distinct_prime_factors k n) |
    n <- [product (take k primes) ..] ]

main :: IO String
main = return $ show $ head $ prob47 4 4
-- 134043

-- 134043 = 3 7 13 491
-- 134044 = 2 2 23 31 47
-- 134045 = 5 17 19 83
-- 134046 = 2 3 3 11 677
