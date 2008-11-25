module Euler078 where
import EulerLib
import List

------------------------------------------------------------------------------
-- 78. Investigating the number of ways in which coins can be separated into piles.
{-
Let p(n) represent the number of different ways in which n coins can be
separated into piles. For example, five coins can separated into piles in
exactly seven different ways, so p(5)=7.
OOOOO
OOOO   O
OOO   OO
OOO   O   O
OO   OO   O
OO   O   O   O
O   O   O   O   O

Find the least value of n for which p(n) is divisible by one million.
-}

{-
A000041:  a(n) = number of partitions of n (the partition numbers).
1, 1, 2, 3, 5, 7, 11, 15, 22, 30, 42, 56, 77, 101, 135, 176, 231, 297, 385, 490, 627, 792, 1002, 1255, 1575, 1958, 2436, 3010, 3718, 4565, 5604, 6842, 8349, 10143, 12310, 14883, 17977, 21637, 26015, 31185, 37338, 44583, 53174, 63261, 75175, 89134 ...
-}

pos_pent n = n*(3*n - 1) `div` 2  -- positive pentagon numbers
neg_pent n = n*(3*n + 1) `div` 2  -- negative pentagon numbers
pos_ks = [ f n | n <- [1,3 ..], f <- [pos_pent, neg_pent] ]
neg_ks = [ f n | n <- [2,4 ..], f <- [pos_pent, neg_pent] ]

partition_numbers_mod :: Int -> [Int]
partition_numbers_mod n = 1 : ps
  where
    ps = map (`mod` n) (f 1 pos_ks neg_ks (repeat 0))
    f i (j:js) (k:ks) (x:xs)
      | i == j = x+1 : zipWith (+) (f (i+1) js (k:ks) xs) ps
      | i == k = x-1 : zipWith (-) (f (i+1) (j:js) ks xs) ps
      | otherwise = x : f (i+1) (j:js) (k:ks) xs

prob78 :: Int -> Int
prob78 n = head $ findIndices (==0) $ partition_numbers_mod n

main :: IO String
main = return $ show $ prob78 (10^6)
-- 55374
