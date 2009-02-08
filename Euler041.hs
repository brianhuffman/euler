module Euler041 where
import Permutation
import Primes

{-
Problem 41
11 April 2003

We shall say that an n-digit number is pandigital if it makes use of
all the digits 1 to n exactly once. For example, 2143 is a 4-digit
pandigital and is also prime.

What is the largest n-digit pandigital prime that exists?
-}

{-
A number with any of these sets of digits is a multiple of 3.
12
123
12345
123456
12345678
123456789

The pandigital prime must have the digits
1234 or 1234567
-}

-- 7-digit pandigital primes, in reverse sorted order
pandigital_primes_7 :: [Int]
pandigital_primes_7 =
  filter is_prime $ map read $ permutations "7654321"

main :: IO String
main = return $ show $ head $ pandigital_primes_7

answer :: String
answer = "7652413"
