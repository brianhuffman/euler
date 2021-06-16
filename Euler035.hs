module Euler035 where
import Primes
import qualified SortedList as S
import Data.List

{-
Problem 35
Circular Primes.

17 January 2003

The number, 197, is called a circular prime because all rotations of
the digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31,
37, 71, 73, 79, and 97.

How many circular primes are there below one million?
-}

{-
The one-digit circular primes are [2, 3, 5, 7].

Circular primes of any other length must contain only the digits 1,3,7,9.

If a prime is a rotation of itself, then it must consist of only ones.
(Since the repeated portion is a factor of the larger number.)

Such primes include 11 and 1111111111111111111 (19 digits), but nothing
in between. We will special-case these.
-}

candidates :: Int -> [String]
candidates 0 = [""]
candidates n = [ d:ds | d <- "1379", ds <- candidates (n-1) ]

rotations :: [a] -> [[a]]
rotations s = drop 1 $ zipWith (++) (tails s) (inits s)

type Z = Int

circles :: Int -> [[Z]]
circles n =
  [ xs |
    ds <- candidates n,
    let rs = rotations ds,
    let xs = map read rs,
    all (head xs <=) xs ]

circular_primes :: Int -> [Z]
circular_primes n = concat
  [ xs | xs <- circles n, all is_prime xs ]

prob35 :: Int -> [Int]
prob35 n = xs ++ ys
  where
    xs = [2,3,5,7,11,13,17,31,37,71,73,79,97]
    ys = concatMap circular_primes [3 .. n]

main :: IO String
main = return $ show $ length $ prob35 6
-- 55
