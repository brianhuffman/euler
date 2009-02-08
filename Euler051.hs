module Euler051 where
import Permutation
import Primes

{-
Problem 51
Find the smallest prime which, by changing the same part of the number,
  can form eight different primes.

29 August 2003

By replacing the 1st digit of *57, it turns out that six of the
possible values: 157, 257, 457, 557, 757, and 857, are all prime.

By replacing the 3rd and 4th digits of 56**3 with the same digit, this
5-digit number is the first example having seven primes, yielding the
family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993.
Consequently 56003, being the first member of this family, is the
smallest prime with this property.

Find the smallest prime which, by replacing part of the number (not
necessarily adjacent digits) with the same digit, is part of an eight
prime value family.
-}

{-
*2*3*3 = [ 020303 + k * 101010 | k <- [1,2,3,4,5,6,8,9] ]

Note: The pattern (101010) must be a multiple of 3.
Otherwise every third value of k will be ruled out, and
there could not possibly be 8 valid choices for k.
-}

type Z = Int

prob51a :: Int -> Int -> Int -> [[Z]]
prob51a m n l =
  [ ps |
    (xs,ys) <- partitionPairs [0 .. n-1],
    length xs == m,
    let mask = sum (map (10^) xs),
    ds <- sequence (replicate (length ys) [0 .. 9]),
    let base = sum (zipWith (\d y -> d * 10^y) ds ys),
    let qs = [ base + k * mask | k <- [0 .. 9] ],
    let ps = filter primeInt qs,
    length ps == l ]

prob51 :: Int -> Z
prob51 l = head
  [ head ps |
    n <- [1 ..],
    m <- [3, 6 .. n-1],
    ps <- prob51a m n l ]

main :: IO String
main = return $ show $ prob51 8
-- *2*3*3  {1,2,3,4,5,6,8,9}

answer :: String
answer = "121313"
