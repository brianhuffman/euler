module Euler187 where
import PrimeArray
import Data.Array.Unboxed

{-
-- precondition: xs in sorted order
products_below :: Int -> [Int] -> Int
products_below m xs = count xs (reverse xs) (length xs) 0
  where
    -- invariant: l = number of elements between x and y
    count (x:xs) (y:ys) l t
      | l == 0    = t
      | x*y > m   = count (x:xs) ys (l-1) t
      | otherwise = count xs (y:ys) (l-1) $! (t+l)

too slow.
TODO: rewrite this algorithm using arrays for speed.
-}

products_below :: Int ->  [Int] -> Int
products_below m xs = count xs 0
  where
    count [] t = t
    count (x:xs) t
      | l == 0    = t
      | otherwise = count xs $! (t+l)
      where
        k = m `div` x
        l = length (takeWhile (<= k) (x:xs))

prob187 :: Int -> Int
prob187 m = products_below m ps
  where
    a = odd_prime_array (m`div`2 - 1)
    ps = 2 : [ 2*n+1 | (n, True) <- assocs a ]

main :: IO String
main = return $ show $ prob187 (10^8)
-- 17427258
