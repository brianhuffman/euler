module Euler034 where
import EulerLib
import List (sort)
import Array

------------------------------------------------------------------------------
-- 34. Find the sum of all numbers which are equal to the sum of the factorial of their digits.

-- See also Problem 30.

fact = listArray ('0','9') (map factorial [0 .. 9])

-- sorted sequences, length <= n, with repetition
sorted_seqs_upto 0 _ = [[]]
sorted_seqs_upto n [] = [[]]
sorted_seqs_upto n (x:xs) =
  map (x:) (sorted_seqs_upto (n-1) (x:xs)) ++ sorted_seqs_upto n xs

length_bound :: (Char -> Integer) -> Int
length_bound f = bound 1
  where
    bound l = if f '9' * toInteger l < 10^l then l else bound (l+1)

prob34 :: (Char -> Integer) -> [Integer]
prob34 f =
  [ s |
    ds <- sorted_seqs_upto (length_bound f) "0123456789",
    length ds > 1,
    let s = sum [ f d | d <- ds ],
    sort (show s) == ds ]

main :: IO String
main = return $ show $ sum $ prob34 (fact!)
-- 40730
