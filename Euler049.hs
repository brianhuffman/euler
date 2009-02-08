module Euler049 where
import Primes
import EulerLib (funArray)
import Data.Array
import Data.List (sort)

{-
Problem 49
01 August 2003

The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
increases by 3330, is unusual in two ways: (i) each of the three terms
are prime, and, (ii) each of the 4-digit numbers are permutations of
one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit
primes, exhibiting this property, but there is one other 4-digit
increasing sequence.

What 12-digit number do you form by concatenating the three terms in
this sequence?
-}

prob49 :: [(Int, Int, Int)]
prob49 =
  [ (a, a+d, a+d+d) |
    a <- takeWhile (<10000) $ dropWhile (<1000) primes,
    let s = sort (show a),
    d <- [2, 4 .. (9999-a)`div`2],
    let (b,c) = (a+d, a+d+d),
    p!b && p!c,
    sort (show b) == s,
    sort (show c) == s]
  where
    p = funArray (1000,9999) is_prime
-- [(1487,4817,8147),(2969,6299,9629)]

main :: IO String
main = return $ show a ++ show b ++ show c 
  where
    (a,b,c) = last prob49

answer :: String
answer = "296962999629"
