module Euler303 where
import EulerLib
import Data.Maybe (fromJust)
import Data.IntSet (IntSet)
import qualified Data.IntSet as S

{-

Problem 303
25 September 2010

For a positive integer n, define f(n) as the least positive multiple of n that, written in base 10, uses only digits ≤ 2.

Thus f(2)=2, f(3)=12, f(7)=21, f(42)=210, f(89)=1121222.

Also, SUM n=[1..100] f(n)/n = 11363107.

Find SUM n=[1..10000] f(n)/n.

-}

slow_fs :: [Integer]
slow_fs = 1 : 2 : [ 10*n + d | n <- slow_fs, d <- [0,1,2] ]

slow_f :: Integer -> Integer
slow_f n = head (filter (\x -> x `mod` n == 0) slow_fs)

fast_bfs :: Int -> [(Int, Integer)]
fast_bfs n = fs
  where
    fs :: [(Int, Integer)]
    fs = (1 `mod` n, 1) : (2 `mod` n, 2) : bfs S.empty fs
    bfs s ((r, m) : ms) =
      if r `S.member` s
      then bfs s ms
      else ((10*r  ) `mod` n, 10*m  ) :
           ((10*r+1) `mod` n, 10*m+1) :
           ((10*r+2) `mod` n, 10*m+2) :
           bfs (S.insert r s) ms
    bfs s [] = []

fast_f :: Int -> Integer
fast_f n = fromJust (lookup 0 (fast_bfs n))

fast_g :: Int -> Integer
fast_g n = fast_f n `div` fromIntegral n

prob303 :: Int -> Integer
prob303 nmax = sum (map fast_g [1..nmax])

main :: IO String
main = return $ show (prob303 10000)

answer :: String
answer = "1111981904675169"
