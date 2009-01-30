module Euler095 where
import PrimeArray (sum_divisors_array)
import Data.Array.Unboxed
import Data.Maybe (maybeToList)

{-
Problem 95
13 May 2005

The proper divisors of a number are all the divisors excluding the
number itself. For example, the proper divisors of 28 are 1, 2, 4, 7,
and 14. As the sum of these divisors is equal to 28, we call it a
perfect number.

Interestingly the sum of the proper divisors of 220 is 284 and the sum
of the proper divisors of 284 is 220, forming a chain of two
numbers. For this reason, 220 and 284 are called an amicable pair.

Perhaps less well known are longer chains. For example, starting with
12496, we form an amicable chain of five numbers:

12496 → 14288 → 15472 → 14536 → 14264 (→ 12496 → ...)

Find the smallest member of the longest amicable chain with no element
exceeding one million.
-}

every_other :: [a] -> [a]
every_other (_:x:xs) = x : every_other xs
every_other _ = []

cycle_length :: Eq a => [a] -> Maybe Int
cycle_length [] = Nothing
cycle_length (x0:xs) = rho 1 (zip xs (every_other xs))
  where
    rho i [] = Nothing
    rho i ((a,b):cs)
      | a /= b = rho (i+1) cs
      | a == x0 = Just i
      | otherwise = Nothing

prob95 :: Int -> [(Int, Int)]
prob95 m =
  [ (-k, n) |
    n <- [1 .. m],
    k <- maybeToList (cycle_length (chain n)) ]
  where
    a = sum_divisors_array m
    chain n =
      takeWhile (\x -> x >= n && x <= m)
        (iterate (\x -> a!x - x) n)

main :: IO String
main = return $ show $ snd $ minimum $ prob95 (10^6)

answer :: String
answer = "14316"
