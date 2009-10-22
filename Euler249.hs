module Euler249 where
import Data.List
import Primes
import Permutation
import EulerLib

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

add :: Integer -> Integer -> Integer
add x y = if z < 10^16 then z else z - 10^16
  where z = x + y

-- sorted by first element
type Counts = [(Int, Integer)]

merge :: Counts -> Counts -> Counts
merge xs@((x,i):xs') ys@((y,j):ys') =
  case compare x y of
    LT -> (x,i) : merge xs' ys
    GT -> (y,j) : merge xs ys'
    EQ -> (x,add i j) : merge xs' ys'
merge xs [] = xs
merge [] ys = ys

shift :: Int -> Counts -> Counts
shift n = map (\(x,i) -> (x+n,i))

combine :: Int -> Counts -> Counts
combine p cs = merge cs (shift p cs)

combine' :: Counts -> Int -> Counts
combine' cs p = merge cs (shift p cs)

result :: Int -> Integer
result pmax = foldl' add 0 prime_totals
  where
    totals :: Counts
    -- totals = foldr combine [(0,1)] (takeWhile (< pmax) primes)
    totals = foldl combine' [(0,1)] (takeWhile (< pmax) primes)
    prime_totals :: [Integer]
    prime_totals = [ i | (n, i) <- totals, is_prime n ]

main :: IO String
main = return $ show $ result 5000

answer :: String
answer = "9275262564250418"
