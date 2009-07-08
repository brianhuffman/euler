module Euler240 where
import Data.Array
import Data.List (zip4, sort)
import EulerLib (pascal_triangle, funArray)

{-
Problem 240
Top Dice

10 April 2009

There are 1111 ways in which five 6-sided dice (sides numbered 1 to 6)
can be rolled so that the top three sum to 15. Some examples are:

D_(1),D_(2),D_(3),D_(4),D_(5) = 4,3,6,3,5
D_(1),D_(2),D_(3),D_(4),D_(5) = 4,3,3,5,6
D_(1),D_(2),D_(3),D_(4),D_(5) = 3,3,3,6,6
D_(1),D_(2),D_(3),D_(4),D_(5) = 6,6,3,3,3

In how many ways can twenty 12-sided dice (sides numbered 1 to 12) be
rolled so that the top ten sum to 70?

-}

{-

f(5,6,3,15) = 1111
f(20,12,10,70) = ?

f(5,6,3,15) =
  choose 5 0 * f(5,5,3,15) +           -- no sixes
  choose 5 1 * f(4,5,2, 9) +           -- 1 six
  choose 5 2 * f(3,5,1, 3)             -- 2 sixes

f(5,5,3,15) =
  choose 5 0 * f(5,4,3,15) +           -- no fives
  choose 5 1 * f(4,4,2,10) +           -- 1 five
  choose 5 2 * f(3,4,1, 5) +           -- 2 fives
  choose 5 3 * f(2,4,0, 0) +           -- 3 fives
  choose 5 4 * f(1,4,0, 0) +           -- 4 fives
  choose 5 5 * f(0,4,0, 0)             -- 5 fives

f(2,2,1,4) =
  choose 2 0 * f(2,1,1,4) +          -- no twos
  choose 2 1 * f(1,1,0,2) +          -- 1 two
  choose 2 2 * f(0,1,0,2)            -- 2 twos

-}

slow240 (n,m,k,t) =
  [ ds |
    ds <- sequence (replicate n [1 .. m]),
    k <= n,
    sum (take k (reverse (sort ds))) == t
  ]

prob240 :: (Int, Int, Int, Int) -> Array (Int, Int, Int, Int) Integer
prob240 (n,m,k,t) = a
  where
    a = funArray ((0,1,0,0),(n,m,k,t)) f
    f (n,m,k,t) | k > n = 0
    f (0,m,k,t) = if t == 0 then 1 else 0
    f (n,1,k,t) = if k == t then 1 else 0
    f (n,m,k,t) = sum (zipWith (*) row xs)
      where
        row = pascal_triangle !! n
        ns = [n, n-1 .. 0]
        ms = repeat (m-1)
        kts = next (k, t)
        next (k, t)
          | k == 0 = (k, t) : next (k, t)
          | m <= t = (k, t) : next (k-1, t-m)
          | otherwise = [(k, t)]
        (ks, ts) = unzip kts
        xs = map (a!) (zip4 ns ms ks ts)

main :: IO String
main = return $ show $ prob240 (20,12,10,70) ! (20,12,10,70)

answer :: String
answer = "7448717393364181966"
