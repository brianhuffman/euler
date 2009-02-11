module Euler146 where
import EulerLib
import Primes
import Data.Array
import Data.Int
import TakeWhile

{-
Problem 146
Investigating a Prime Pattern

24 March 2007

The smallest positive integer n for which the numbers n^2+1, n^2+3,
n^2+7, n^2+9, n^2+13, and n^2+27 are consecutive primes is 10. The sum
of all such integers n below one-million is 1242490.

What is the sum of all such integers n below 150 million?
-}

-- possible values of n mod p such that none of n^2 + [1,3,7,9,13,27]
-- are equivalent to 0 mod p.
possible_mod p =
  [ n | n <- [0 .. p-1],
    not $ any (divides p) $ map (n^2 +) [1,3,7,9,13,27] ]

{-
Problem: find n such that
n^2 + [1,3,7,9,13,27]  are prime
n^2 + [5,11,15,17,19,21,23,25]  are composite

Based on prime requirements:
n == [0]  (mod 2)
n == [1,2]  (mod 3)
n == [0]  (mod 5)
n == [3,4]  (mod 7)
n == [1,3,4,9,10,12]  (mod 13)

Then n^2 == 100  (mod 210)

7 divides n^2 + [5,19]
5 divides n^2 + [5,15,25]
3 divides n^2 + [5,11,17,23]
(only need to check that n^2 + 21 is composite)

n == 10 or 60  (mod 70)
n == [10,80,130,200]  (mod 210)
(still almost 3 million values to check)

n == [10,220,270,290,360,430,480,550,620,640,690,900]  (mod 910)
(12 numbers out of every 910)
(still about 2 million values to check)

n == +/- [10,220,290,430,550,620,640,920,1130,1180,1270,1340]  (mod 2730)
(24 numbers out of every 2730)
(nearly 1.32 million out of 150 million)
-}

type Z = Int64

-- all possible values of n tested with all primes up to 40
candidates40 :: Int -> [Z]
candidates40 m = map fromIntegral (takeWhile (< m) (filter ok ns))
  where
    ns = [ n+k | n <- [0, 910 ..],
                 k <- [10,220,270,290,360,430,480,550,620,640,690,900] ]
    a11 = listArray (0,10) (map (=='1') "11001111001")
    a17 = listArray (0,16) (map (=='1') "11010011111100101")
    a19 = listArray (0,18) (map (=='1') "1111001011110100111")
    a23 = listArray (0,22) (map (=='1') "11110111111111111110111")
    a29 = listArray (0,28) (map (=='1') "11110110111101001011110110111")
    a31 = listArray (0,30) (map (=='1') "1101111011101111111101110111101")
    a37 = listArray (0,36) (map (=='1') "1111110111101111000000111101111011111")
    ok n =
      a11!(n `mod` 11) &&
      a17!(n `mod` 17) &&
      (n `mod` 3 /= 0) &&
      a19!(n `mod` 19) &&
      a29!(n `mod` 29) &&
      a37!(n `mod` 37) &&
      a31!(n `mod` 31) &&
      a23!(n `mod` 23)
-- about 1.6 sec to calculate candidates40 (150*10^6)
-- 144524 remain

candidates1000 :: Int -> [Z]
candidates1000 m = 10 : filter ok (candidates40 m)
  where
    ps = takeWhile (< 1000) $ dropWhile (< 40) primes
    ok n = all (check (n^2)) ps
    a = funArray (0, 27) (`notElem` [1,3,7,9,13,27])
    check n2 p
      | r > 27 = True
      | otherwise = a ! r
      where r = p - (n2 `mod` p)
-- about 2.4 sec to calculate candidates1000 (150*10^6)
-- 4418 remain

prob146 :: Int -> [Z]
prob146 m = filter ok (candidates1000 m)
  where
    ok n = and [ test (n^2 + k) | k <- [1,3,7,9,13,27] ]
           && not (test (n^2 + 21))
    test n = all (\a -> miller_rabin_base a (toInteger n)) [2,3,5,7,11,13,17]

-- prob146 (150*10^6) = [10, 315410, 927070, 2525870, 8146100, 16755190, 39313460, 97387280, 119571820, 121288430, 130116970, 139985660]

main :: IO String
main = return $ show $ sum $ prob146 (150*10^6)

answer :: String
answer = "676333270"
