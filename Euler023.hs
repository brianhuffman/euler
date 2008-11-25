module Euler023 where
import PrimeArray
import Data.Array.Unboxed

{-
Problem 23
Sums of two abundant numbers.

02 August 2002

A perfect number is a number for which the sum of its proper divisors is
exactly equal to the number. For example, the sum of the proper divisors
of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect
number.

A number whose proper divisors are less than the number is called deficient
and a number whose proper divisors exceed the number is called abundant.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
number that can be written as the sum of two abundant numbers is 24. By
mathematical analysis, it can be shown that all integers greater than 28123
can be written as the sum of two abundant numbers. However, this upper limit
cannot be reduced any further by analysis even though it is known that the
greatest number that cannot be expressed as the sum of two abundant numbers
is less than this limit.

Find the sum of all the positive integers which cannot be written as the sum
of two abundant numbers.
-}

abundants :: Int -> [Int]
abundants m = [ n | n <- [1 .. m], 2*n < s!n ]
  where s = sum_divisors_array m

abundant_sums_array :: Int -> UArray Int Bool
abundant_sums_array m =
  accumArray (const id) False (1, m)
    [ (x+y, True) | x <- xs, y <- takeWhile (<= min x (m-x)) xs ]
  where xs = abundants m

not_abundant_sums :: Int -> [Int]
not_abundant_sums m = [ n | n <- [1 .. m], not (a!n) ]
  where a = abundant_sums_array m

prob23 :: Int -> Int
prob23 m = sum (not_abundant_sums m)

limit :: Int
limit = 28123

main :: IO String
main = return $ show $ prob23 limit
-- 4179871
