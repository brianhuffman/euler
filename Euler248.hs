module Euler248 where
import Primes
import Permutation
import EulerLib
import qualified SortedList as S
import Data.Array
import Data.List (insert, sort)

title :: String
title = "Totient Equals 13!"

{---------------------------------------------------------------------
Problem 248
06 June 2009

The first number n for which φ(n)=13! is 6227180929.

Find the 150,000^(th) such number.

---------------------------------------------------------------------}

{-
6227180929 = 66529 * 93601

phi(66529) = 2^5 * 3^3 * 7 * 11
phi(93601) = 2^5 * 3^2 * 5^2 * 13

13! = 2^10 * 3^5 * 5^2 * 7 * 11 * 13

13! has 1584 factors.

459 of the factors of 13! are one less than a prime.

Thus there are 459 primes p such that phi(p) | 13!

Other prime powers such that phi(p^n) | 13!:

phi(13^2) = 2^2 * 3 * 13
phi(11^2) = 2 * 5 * 11
phi(7^2) = 2 * 3 * 7
phi(5^2) = 2^2 * 5
phi(5^3) = 2^2 * 5^2
phi(3^2) = 2 * 3
...
phi(3^6) = 2 * 3^5
phi(2^2) = 2
...
phi(2^11) = 2^10

-}

type PF = ((Int, Int, Int), (Int, Int, Int))

pf13 :: PF
pf13 = ((10, 5, 2), (1, 1, 1))

pf0 :: PF
pf0 = ((0, 0, 0), (0, 0, 0))

lePF :: PF -> PF -> Bool
lePF ((x2,x3,x5),(x7,x11,x13)) ((y2,y3,y5),(y7,y11,y13)) =
  and [x2 <= y2, x3 <= y3, x5 <= y5, x7 <= y7, x11 <= y11, x13 <= y13]

diffPF :: PF -> PF -> PF
diffPF ((x2,x3,x5),(x7,x11,x13)) ((y2,y3,y5),(y7,y11,y13)) =
  ((x2-y2, x3-y3, x5-y5), (x7-y7, x11-y11, x13-y13))

almostPF :: PF -> PF -> Bool
almostPF ((x2,x3,x5),(x7,x11,x13)) ((y2,y3,y5),(y7,y11,y13))
  | y13 > 0 = x13 > 0
  | y11 > 0 = x11 > 0
  | y7 > 0 = x7 > 0
  | y5 > 0 = x5 > 0
  | y3 > 0 = x3 > 0
  | y2 > 0 = x2 > 0

pfToZ :: PF -> Integer
pfToZ ((n2, n3, n5), (n7, n11, n13)) =
  2^n2 * 3^n3 * 5^n5 * 7^n7 * 11^n11 * 13^n13

f13 :: Integer
f13 = factorial 13

ds :: [Integer]
ds = list_divisors f13

-- (pf, p) <- totients,
-- pfToZ pf = totient p
totients1 :: [(PF, Integer)]
totients1 =
  [ (pf, p) |
    n2 <- [0 .. 10],
    n3 <- [0 .. 5],
    n5 <- [0 .. 2],
    n7 <- [0 .. 1],
    n11 <- [0 .. 1],
    n13 <- [0 .. 1],
    let pf = ((n2, n3, n5), (n7, n11, n13)),
    let p = pfToZ pf + 1,
    is_prime p ]

totients2 :: [(PF, Integer)]
totients2 = sort
  [ (((1, 0, 0), (0, 0, 0)), 2^2),
    (((2, 0, 0), (0, 0, 0)), 2^3),
    (((3, 0, 0), (0, 0, 0)), 2^4),
    (((4, 0, 0), (0, 0, 0)), 2^5),
    (((5, 0, 0), (0, 0, 0)), 2^6),
    (((6, 0, 0), (0, 0, 0)), 2^7),
    (((7, 0, 0), (0, 0, 0)), 2^8),
    (((8, 0, 0), (0, 0, 0)), 2^9),
    (((9, 0, 0), (0, 0, 0)), 2^10),
    (((10, 0, 0), (0, 0, 0)), 2^11),
    (((1, 1, 0), (0, 0, 0)), 3^2),
    (((1, 2, 0), (0, 0, 0)), 3^3),
    (((1, 3, 0), (0, 0, 0)), 3^4),
    (((1, 4, 0), (0, 0, 0)), 3^5),
    (((1, 5, 0), (0, 0, 0)), 3^6),
    (((2, 0, 1), (0, 0, 0)), 5^2),
    (((2, 0, 2), (0, 0, 0)), 5^3),
    (((1, 1, 0), (1, 0, 0)), 7^2),
    (((1, 0, 1), (0, 1, 0)), 11^2),
    (((2, 1, 0), (0 ,0, 1)), 13^2) ]

totients3 :: [(PF, [Integer])]
totients3 = merge totients1 totients2
  where
    merge xs [] = map (\(pf, n) -> (pf, [n])) xs
    merge [] ys = map (\(pf, n) -> (pf, [n])) ys
    merge xs@((pf1,x):xs') ys@((pf2,y):ys') =
      case compare pf1 pf2 of
        EQ -> (pf1, sort [x, y]) : merge xs' ys'
        LT -> (pf1, [x]) : merge xs' ys
        GT -> (pf2, [y]) : merge xs ys'

array3 :: Array PF [Integer]
array3 = a
  where
    a = funArray (pf0, pf13) f
    f ((0, 0, 0), (0, 0, 0)) = [1, 2]
    f pfn =
      foldl S.union []
        [ ns |
          (pfp, ps) <- totients3,
          pfp `lePF` pfn,
          pfp `almostPF` pfn,
          let pfm = pfn `diffPF` pfp,
          let ms = a ! pfm,
          let ns = foldl S.union []
                [ [ p * m | m <- ms, gcd p m == 1 ] | p <- ps ]
        ]

main :: IO String
main = return $ show $ (array3 ! pf13) !! (150000 - 1)

answer :: String
answer = "23507044290"

space :: Int
space = 3 --MB

time :: Double
time = 2.3 --sec

gctime :: Double
gctime = 0.6 --sec
