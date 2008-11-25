module Euler095 where
import PrimeArray (sum_divisors_array)
import Data.Array.Unboxed
import EulerLib
import Primes
import Maybe (maybeToList)
--import Euler021

------------------------------------------------------------------------------
-- 95. Find the smallest member of the longest amicable chain with no element exceeding one million.
{-
The proper divisors of a number are all the divisors excluding the number
itself. For example, the proper divisors of 28 are 1, 2, 4, 7, and 14. As
the sum of these divisors is equal to 28, we call it a perfect number.

Interestingly the sum of the proper divisors of 220 is 284 and the sum of
the proper divisors of 284 is 220, forming a chain of two numbers. For this
reason, 220 and 284 are called an amicable pair.

Perhaps less well known are longer chains. For example, starting with 12496,
we form an amicable chain of five numbers:

12496 -> 14288 -> 15472 -> 14536 -> 14264 (-> 12496 -> ...)

Find the smallest member of the longest amicable chain with no element
exceeding one million.
-}

nmax :: Int
nmax = 10^6

sum_proper_divisors :: Int -> Int
sum_proper_divisors = (\n -> a ! n - n)
  where a = sum_divisors_array nmax

amicable_chain :: Int -> [Int]
amicable_chain n
  | n > nmax || n < 1 = []
  | otherwise = n : amicable_chain (sum_proper_divisors n)

every_other :: [a] -> [a]
every_other (_:x:xs) = x : every_other xs
every_other _ = []

amicable_degree :: Int -> Maybe Int
amicable_degree n = f cs 1
  where
    xs = drop 1 (amicable_chain n)
    cs = zip xs (every_other xs)
    f [] i = Nothing
    f ((a,b):cs) i
      | a /= b = f cs (i+1)
      | a == n = Just i
      | otherwise = Nothing

-- TODO: parameterize by nmax
prob95 :: [(Int, Int)]
prob95 =
  [ (-k, n) |
    n <- [1 .. nmax],
    k <- maybeToList (amicable_degree n) ]

main :: IO String
main = return $ show $ snd $ minimum prob95
-- 14316
