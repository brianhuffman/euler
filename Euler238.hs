{-# LANGUAGE BangPatterns #-}
module Euler238 where
import Primes
import Data.Char (digitToInt)
import Data.Array.Unboxed
import Data.Array.ST

title :: String
title = "Infinite String Tour"

{---------------------------------------------------------------------
Problem 238
27 March 2009

Create a sequence of numbers using the "Blum Blum Shub" pseudo-random
number generator:

s_(0)   = 14025256
s_(n+1) = s_(n)^(2) mod 20300713

Concatenate these numbers  s_(0)s_(1)s_(2)… to create a string w of
infinite length.

Then, w = 14025256741014958470038053646…

For a non-negative integer k, if no substring of w exists with a sum
of digits equal to k, p(k) is defined to be zero. If at least one
substring of w exists with a sum of digits equal to k, we define
p(k) = z, where z is the starting position of the earliest such
substring.

For instance:

The substrings 1, 14, 1402, …
with respective sums of digits equal to 1, 5, 7, …
start at position 1, hence p(1) = p(5) = p(7) = … = 1.

The substrings 4, 402, 4025, …
with respective sums of digits equal to 4, 6, 11, …
start at position 2, hence p(4) = p(6) = p(11) = … = 2.

The substrings 0, 02, 0252, …
with respective sums of digits equal to 0, 2, 9, …
start at position 3, hence p(0) = p(2) = p(9) = … = 3.

Note that substring 025 starting at position 3, has a sum of digits
equal to 7, but there was an earlier substring (starting at position
1) with a sum of digits equal to 7, so p(7) = 1, not 3.

We can verify that, for 0 ≤ k ≤ 10^(3), ∑ p(k) = 4745.

Find ∑ p(k), for 0 ≤ k ≤ 10^(15).

---------------------------------------------------------------------}

seed :: Integer
seed = 14025256

bbs :: Integer -> [Integer]
bbs !n = n : bbs (n * n `mod` 20300713)

{---------------------------------------------------------------------
s_(0)   = 14025256
s_(n+1) = s_(n)^(2) mod 20300713

20300713 = 4127 * 4919

multiplicative order of 1402526 (mod 20300713) = 5072917
multiplicative order of 1402526 (mod 4127) = 2063
multiplicative order of 1402526 (mod 4919) = 2459

bbs (mod 4127) loops with period 1031.
bbs (mod 4919) loops with period 2458.
bbs loops with period 2534198 = lcm 1031 2458.

Number of digits in repeating portion = 18886117.
Sum    of digits in repeating portion = 80846691.

p(k + 80846691) = p(k), for k > 0.
p(80846691) = 1.
p(0) = 3.

---------------------------------------------------------------------}

-- bbs1 loops with period 1031
bbs1 :: Integer -> [Integer]
bbs1 !n = n : bbs1 (n * n `mod` 4127)

-- bbs2 loops with period 2458
bbs2 :: Integer -> [Integer]
bbs2 !n = n : bbs2 (n * n `mod` 4919)

-- bbs loops with period 2534198 = lcm 1031 2458
bbs_cycle :: [Integer]
bbs_cycle = take 2534198 (bbs seed)

digit_sum :: Integer -> Int
digit_sum = sum . map digitToInt . show

-- sum_digits = 80846691
sum_digits :: Int
sum_digits = sum (map digit_sum bbs_cycle)

-- length_digits = 18886117
length_digits :: Int
length_digits = sum (map (length . show) bbs_cycle)

----------------------------------------------------------------------

{-
FIXME: nicer version causes memory leak.

digit_array :: UArray Int Char
digit_array = listArray (1, 18886117) digits
  where digits = concatMap show (take 2534198 (bbs seed))

digit_sums_array :: UArray Int Int
digit_sums_array = listArray (0, 18886117) (scanl (+) 0 digits)
  where digits = map digitToInt (elems digit_array)
-}

digit_sums_array :: UArray Int Int
digit_sums_array = runSTUArray (
  do a <- newArray (0, 18886117) 0
     let go i t cs n | i > 18886117 = return ()
         go i t [] n = go i t (show n) (n * n `mod` 20300713)
         go i t (c:cs) n = do
           writeArray a i t
           go (i+1) (t + digitToInt c) cs n
     go 0 0 [] seed
     return a
  )

-- p(k) for 0 <= k <= 80,846,691
offset_array :: UArray Int Int
offset_array = accumArray (min) 9999999 (1, 80846691)
  [ (k, i) |
    i <- [1 .. 20],
    let s0 = digit_sums_array ! (i-1),
    j <- [1 .. 18886117],
    let s1 = digit_sums_array ! j,
    let k = (s1 - s0) `mod` 80846691
  ]

main :: IO String
main = return $ show $ answer

answer :: String
answer = "9922545104535661"


{-
SUM p(k) for k = 1..80,846,691 = 401,102,469


-}

