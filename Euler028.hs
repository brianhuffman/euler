module Euler028 where

------------------------------------------------------------------------------
-- 28. What is the sum of both diagonals in a 1001 by 1001 spiral?

spiral_seq = scanl (+) 1 $ concatMap (replicate 4) [2,4..]

prob28 :: Integer -> Integer
prob28 n = sum $ takeWhile (<= n * n) spiral_seq
-- prob28 1001 = 669171001

main :: IO String
main = return $ show $ prob28 1001
