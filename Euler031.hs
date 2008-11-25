module Euler031 where

------------------------------------------------------------------------------
-- 31. Investigating combinations of English currency denominations.

new_coin :: Int -> [Integer] -> [Integer]
new_coin d xs = ys
  where ys = zipWith (+) xs (replicate d 0 ++ ys)

no_coins :: [Integer]
no_coins = 1 : repeat 0

all_coins :: [Int] -> [Integer]
all_coins = foldr new_coin no_coins

prob31 :: Int -> Integer
prob31 n = all_coins [1,2,5,10,20,50,100,200] !! n

main :: IO String
main = return $ show $ prob31 200
-- 73682
