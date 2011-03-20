module Euler326 where
import Data.Array
import Data.Int

{-
Modulo Summations

Problem 326
26 February 2011

Let a[n] be a sequence recursively defined by:

  * a[1] = 1
  * a[n] = SUM k=[1..n-1] k*a[k] mod n

So the first 10 elements of a[n] are: 1,1,0,3,0,3,5,4,1,9.

Let f(N,M) represent the number of pairs (p,q) such that:

  1 ≤ p ≤ q ≤ N  and  SUM i=[p..q] a[i] mod M = 0

It can be seen that f(10,10)=4 with the pairs (3,3), (5,5), (7,9) and
(9,10).

You are also given that f(10^4,10^3)=97158.

Find f(10^12,10^6).

-}


seq_a = 1 : f 1 2
  where f s n = let a = s`mod`n in a : f (s + a*n) (n+1)

{-
a[1,7..] = [1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65,69,73,77]
a[2,8..] = [1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,52,55,58]
a[3,9..] = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
a[4,10..] = [3,9,15,21,27,33,39,45,51,57,63,69,75,81,87,93,99,105,111,117]
a[5,11..] = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
a[6,12..] = [3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60]

a[6n+1..6n+6] = [4*n+1, 3*n+1, n, 6*n+3, n, 3*n+3]
-}

-- seq_a' = seq_a
seq_a' = [ a | n <- [0..], a <- [4*n+1, 3*n+1, n, 6*n+3, n, 3*n+3] ]

{-

SUM i=[p..q] a[i]

SUM i=[6n+1..6n+1] a[i] = 4*n+1
SUM i=[6n+1..6n+2] a[i] = 7*n+2
SUM i=[6n+1..6n+3] a[i] = 8*n+2
SUM i=[6n+1..6n+4] a[i] = 14*n+5
SUM i=[6n+1..6n+5] a[i] = 15*n+5
SUM i=[6n+1..6n+6] a[i] = 18*n+8

SUM i=[1..6n] a[i]
= SUM k=[0..n-1] (18*k+8)
= 8*n + SUM k=[0..n-1] (18*k)
= 8*n + 18 * SUM k=[0..n-1] k
= 8*n + 18 * (n-1)*n/2
= 8*n + 9*(n-1)*n
= 8*n + 9*n^2 - 9*n
= 9*n^2 - n

SUM i=[1..6n+0] a[i] = 9n^2 - n
SUM i=[1..6n+1] a[i] = 9n^2 + 3n + 1
SUM i=[1..6n+2] a[i] = 9n^2 + 6n + 2
SUM i=[1..6n+3] a[i] = 9n^2 + 7n + 2
SUM i=[1..6n+4] a[i] = 9n^2 + 13n + 5
SUM i=[1..6n+5] a[i] = 9n^2 + 14n + 5

-}

-- sum_a n = sum (take n seq_a)
sum_a n = 9*q^2 - q + [0, 4*q+1, 7*q+2, 8*q+2, 14*q+5, 15*q+5] !! r
  where (q, r) = divMod n 6

{-

f(10,10) = 4:

[ sum_a n `mod` 10 | n <- [0..10] ]
= [0,1,2,2,5,5,8,3,7,8,7]

There are:
  2 2's (1 pair)
  2 5's (1 pair)
  2 7's (1 pair)
  2 8's (1 pair)

If a remainder occurs k times, this adds k*(k-1)/2 pairs.

Strategy: count all remainders separately, and add k*(k-1)/2 over all
counts.

When does SUM i=[1..n] a[i] == r (mod m)?

9k^2 - k == r (mod m)
9k^2 + 3k + 1 == r (mod m)
9k^2 + 6k + 2 == r (mod m)
9k^2 + 7k + 2 == r (mod m)
9k^2 + 13k + 5 == r (mod m)
9k^2 + 14k + 5 == r (mod m)
-}

-- for how many n <= nmax does sum_a n `mod` m == r?
count_remainder :: Int64 -> Int -> Array Int Int
count_remainder nmax m =
  accumArray (+) 0 (0, m-1)
    [ (r, x) |
      k <- [0..m'-1],
      (d, s) <- [(0, 9*k^2 - k),
                 (1, 9*k^2 + 3*k + 1),
                 (2, 9*k^2 + 6*k + 2),
                 (3, 9*k^2 + 7*k + 2),
                 (4, 9*k^2 + 13*k + 5),
                 (5, 9*k^2 + 14*k + 5)],
      let r = fromIntegral (s `mod` m'),
      -- for how many i is (6*(k+i*m)+d) <= nmax?
      -- for how many i is 6*k+6*i*m+d <= nmax?
      -- for how many i is 6*i*m <= nmax - 6*k+d?
      let x = fromIntegral ((nmax - (6*k+d))`div`(6*m') + 1) ]
  where m' = (fromIntegral m :: Int64)

prob326 :: Int64 -> Int -> Integer
prob326 nmax m = sum [ x*(x-1) | x <- map toInteger (elems a) ] `div` 2
  where a = count_remainder nmax m

main :: IO String
main = return $ show $ prob326 (10^12) (10^6)

answer :: String
answer = "1966666166408794329"
