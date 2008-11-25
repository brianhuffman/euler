module Euler077 where
import Memoize
import Primes
import Data.Int (Int64)

------------------------------------------------------------------------------
-- 77. What is the first value which can be written as the sum of primes in over five thousand different ways?

-- prob77a k (n, m) = 
-- how many ways to write n as sum of primes,
-- using only the first m primes
prob77a :: (Int, Int) -> (Int, Int) -> Int64
prob77a (k,l) = ways'
  where
    ways' = memoizeU ((0,1),(k,l)) ways
    ways (0, m) = 1
    ways (n, m) = sum $ map ways' $
      zip [ n-p | p <- takeWhile (<=n) primes ] [1 .. m]
{-
    ways (n, 0) = 0
    ways (n, m)
      | p <= n    = ways (n, m-1) + ways (n-p, m)
      | otherwise = ways (n, m-1)
      where p = primes !! (m-1)
-}

-- how many ways can k be written as sum of primes
prime_sums :: Int -> Int64
prime_sums k = prob77a (k,l) (k,l)
  where l = k `div` 2

prob77 :: Int64 -> Int
prob77 m = head [ k | k <- [1 ..], prime_sums k > m ]

main :: IO String
main = return $ show $ prob77 5000
-- 71
