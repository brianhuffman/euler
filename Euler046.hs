module Euler046 where
import EulerLib
import Primes
import qualified SortedList as S

------------------------------------------------------------------------------
-- 46. What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

odd_composites :: [Int]
odd_composites = filter odd $ S.deleteFirsts [2 ..] primes

prime_plus_twice_square :: [Int]
prime_plus_twice_square =
  S.big_union [ [ p + 2 * square x | p <- primes ] | x <- [1..] ]

main :: IO String
main =
  return $ show $ head $
  S.deleteFirsts odd_composites prime_plus_twice_square
-- 5777

