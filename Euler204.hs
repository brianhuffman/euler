module Euler204 where
import EulerLib
import Primes

{-
A Hamming number is a positive number which has no prime factor larger than 5.
So the first few Hamming numbers are 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15.
There are 1105 Hamming numbers not exceeding 10^8.

We will call a positive number a generalised Hamming number of type n, if it
has no prime factor larger than n.
Hence the Hamming numbers are the generalised Hamming numbers of type 5.

How many generalised Hamming numbers of type 100 are there which don't exceed
10^9?
-}

-- hammings ps m = {x. x <= m and all prime factors of x are in ps}
hammings [] _ = [1]
hammings _ 0 = []
hammings _ 1 = [1]
hammings (p : ps) m =
  hammings ps m ++
  map (* p) (hammings (p : ps) (m `div` p))

count_hammings _ 0 = 0
count_hammings _ 1 = 1
count_hammings [] _ = 1
count_hammings (p : ps) m =
  count_hammings ps m +
  count_hammings (p : ps) (m `div` p)

prob204 n m = count_hammings ps m
  where ps = reverse $ takeWhile (<= n) primes

main :: IO String
main = return $ show $ prob204 100 (10^9)
