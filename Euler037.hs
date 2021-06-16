module Euler037 where
import EulerLib
import Primes
import qualified SortedList as S
import Data.List

------------------------------------------------------------------------------
-- 37. Find the sum of all eleven primes that are both truncatable from left to right and right to left.

right_truncatable_primes :: [Int]
right_truncatable_primes = xs
  where
    xs = S.intersectBy f primes (2:3:5:7:xs)
    f x y = compare (x`div`10) y

left_truncatable :: Int -> Bool
left_truncatable n =
  all (\m -> 1 < m && is_prime m) $
  map read $
  takeWhile (not . null) $
  tails $ show n

main :: IO String
main = return $ show $ sum $ take 11 $
  filter left_truncatable right_truncatable_primes
-- sum [23,37,53,73,313,317,373,797,3137,3797,739397]
-- 748317

