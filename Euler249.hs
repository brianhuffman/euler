module Euler249 where
import Data.List
import Primes
import Permutation
import EulerLib
import Data.Int
import Data.Array.Unboxed

{-
Problem 249
Prime Subset Sums

13 June 2009

Let S = {2, 3, 5, ..., 4999} be the set of prime numbers less than
5000.

Find the number of subsets of S, the sum of whose elements is a prime
number.  Enter the rightmost 16 digits as your answer.

-}

{-

There 669 primes less than 5000; their sum is 1,548,136.

-}

type Z = Int64

add :: Z -> Z -> Z
add x y = if z < 10^16 then z else z - 10^16
  where z = x + y

type Counts = UArray Int Z

combine :: Counts -> Int -> Counts
combine c n = array (l,u+n) [ (i, f i) | i <- [l .. u+n] ]
  where
    (l,u) = bounds c
    f i
      | i < l+n   = if i > u then 0 else c ! i
      | i > u     = c ! (i-n)
      | otherwise = add (c ! (i-n)) (c ! i)

empty :: Counts
empty = array (0,0) [(0,1)]

result :: Int -> Z
result pmax = foldl' add 0 prime_totals
  where
    totals :: Counts
    totals = foldl combine empty (takeWhile (< pmax) primes)
    prime_totals :: [Z]
    prime_totals = [ i | (n, i) <- assocs totals, is_prime n ]

main :: IO String
main = return $ show $ result 5000

answer :: String
answer = "9275262564250418"
