module Euler087 where
import Primes
import qualified SortedList as S

------------------------------------------------------------------------------
-- 87.  Investigating numbers that can be expressed as the sum of a prime square, cube, and fourth power?

prob87 :: Int -> Int
prob87 n = length $ takeWhile (< n) ps234
  where
    sums xs ys = S.big_union [ [ x+y | x <- xs ] | y <- ys ]
    ps2 = map (^2) primes
    ps3 = map (^3) primes
    ps4 = map (^4) primes
    ps23 = sums ps2 ps3
    ps234 = sums ps23 ps4

main :: IO String
main = return $ show $ prob87 50000000
-- 1097343

