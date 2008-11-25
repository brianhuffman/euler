module Lagged where

-- Lagged Fibonacci Generator

-- Pseudo-random 6-digit integers

-- Used by Problems 149, 186, 212

lagged_fib_init :: [Int]
lagged_fib_init = map (fromInteger . f) [1 .. 55]
  where
    f :: Integer -> Integer
    f k = (100003 - 200003*k + 300007*k^3) `mod` 1000000

lagged_fibonacci :: [Int]
lagged_fibonacci = xs
  where
    xs = lagged_fib_init ++ zipWith add xs (drop 31 xs)
    add x y = if z < 1000000 then z else z - 1000000
      where z = x + y
