module Euler092 where
import EulerLib
import Data.Array.Unboxed
import Data.Char

------------------------------------------------------------------------------
-- 92. Investigating a square digits number chain with a surprising property.
{-
A number chain is created by continuously adding the square of the digits in a
number to form a new number until it has been seen before.

For example,

44 -> 32 -> 13 -> 10 -> 1 -> 1
85 -> 89 -> 145 -> 42 -> 20 -> 4 -> 16 -> 37 -> 58 -> 89

Therefore any chain that arrives at 1 or 89 will become stuck in an endless
loop. What is most amazing is that EVERY starting number will eventually
arrive at 1 or 89.

How many starting numbers below ten million will arrive at 89?
-}

-- Part 1: Build a list of possible sums, with multiplicities

type MList = [(Int, Int)]

merge :: MList -> MList -> MList
merge xs [] = xs
merge [] ys = ys
merge (xs@((x,i):xs')) (ys@((y,j):ys')) =
  case compare x y of
    EQ -> (x, i+j) : merge xs' ys'
    LT -> (x,i) : merge xs' ys
    GT -> (y,j) : merge xs ys'

shift :: Int -> MList -> MList
shift n xs = [ (x+n, i) | (x, i) <- xs ]

sums :: Int -> MList
sums k = iterate next init !! k
  where
    squares = map square [0 .. 9]
    init = [(0, 1)]
    next xs = foldl1 merge [ shift n xs | n <- squares ]

-- Part 2: Find which of these arrive at 89

sum_square_digits :: Int -> Int
sum_square_digits = sum . map (square . digitToInt) . show

arrives_at_89 :: Int -> Bool
arrives_at_89 n
  | n < 2 = False
  | n == 89 = True
  | otherwise = arrives_at_89 (sum_square_digits n)

prob92 :: Int -> Int
prob92 k = sum [ i | (x, i) <- sums k, arrives_at_89 x ]

main :: IO String
main = return $ show $ prob92 7
-- 8581146
