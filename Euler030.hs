module Euler030 where
import Data.Char (digitToInt)
import Data.List (sort)

------------------------------------------------------------------------------
-- 30. Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

-- See also Problem 34.

-- sorted sequences, length <= n, with repetition
sorted_seqs_upto 0 _ = [[]]
sorted_seqs_upto n [] = [[]]
sorted_seqs_upto n (x:xs) =
  map (x:) (sorted_seqs_upto (n-1) (x:xs)) ++ sorted_seqs_upto n xs

length_bound :: Int -> Int
length_bound n = f 1
  where
    f l = if 9^n * toInteger l < 10^l then l else f (l+1)

prob30 :: Int -> [Integer]
prob30 n =
  [ s |
    ds <- sorted_seqs_upto (length_bound n) "0123456789",
    length ds > 1,
    let s = sum [ toInteger (digitToInt d) ^ n | d <- ds ],
    sort (show s) == ds ]

main :: IO String
main = return $ show $ sum $ prob30 5
-- 443839
