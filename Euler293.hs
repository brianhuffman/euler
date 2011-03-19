module Euler293 where
import qualified SortedList as S
import Primes
import Data.List

{-

Problem 293
Pseudo-Fortunate Numbers
22 May 2010

An even positive integer N will be called admissible, if it is a power
of 2 or its distinct prime factors are consecutive primes.

The first twelve admissible numbers are 2,4,6,8,12,16,18,24,30,32,36,48.

If N is admissible, the smallest integer M > 1 such that N+M is prime,
will be called the pseudo-Fortunate number for N.

For example, N=630 is admissible since it is even and its distinct
prime factors are the consecutive primes 2,3,5 and 7.

The next prime number after 631 is 641; hence, the pseudo-Fortunate
number for 630 is M=11.

It can also be seen that the pseudo-Fortunate number for 16 is 3.

Find the sum of all distinct pseudo-Fortunate numbers for admissible
numbers N less than 10^9.

-}

admissibles :: [Int]
admissibles = S.big_union (scanl next xs2 (tail primesInt))
  where
    xs2 = iterate (*2) 2
    next xs p = ys
      where ys = map (*p) (head xs : S.union (tail xs) ys)

pseudo_fortunate n =
  head [ k | k <- [3,5..], primeInt (n+k) ]

prob293 m = sum (nub (map pseudo_fortunate (takeWhile (<m) admissibles)))

main :: IO String
main = return $ show $ prob293 (10^9)

answer :: String
answer = "2209"
