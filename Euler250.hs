module Euler250 where
import Data.Array.Unboxed
import Data.List (foldl')
import Data.Int
import Primes (expMod)
import Permutation


{-
Problem 250
250250

13 June 2009

Find the number of non-empty subsets of {1^(1), 2^(2), 3^(3),...,
250250^(250250)}, the sum of whose elements is divisible by 250. Enter
the rightmost 16 digits as your answer.

-}

numbers' :: [Integer]
numbers' = [ expMod x x 250 | x <- [1 .. 250250] ]

numbers :: [Int]
numbers = map fromInteger numbers' -- [ expMod x x 250 | x <- [1 .. 250250] ]

numbers0 :: [Int]
numbers0 = [ expMod x x 250 | x <- [1 .. 250250] ]
  -- this overflows somewhere; TODO: find problem

unsplice :: [a] -> ([a], [a])
unsplice = foldr (\x (xs, ys) -> (x:ys, xs)) ([], [])

type Z = Int64
type T = UArray Int Z

basecase :: T
basecase = accumArray (const id) 0 (0,249) [(0, 1)]

add :: Z -> Z -> Z
add x y = if z < 10^16 then z else z - 10^16
  where z = x + y

step :: T -> Int -> T
step t n = accum add t [ ((n+i)`mod`250, t!i) | i <- [0..249] ]

sums250 :: [Int] -> T
sums250 = foldl' step basecase

main :: IO String
main = return $ show $ (sums250 numbers ! 0) - 1

answer :: String
answer = "1425480602091519"

-- 4845538099920895 WRONG!
