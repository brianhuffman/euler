module Euler049 where
import EulerLib
import Primes
import Array
import List

------------------------------------------------------------------------------
-- 49. Find arithmetic sequences, made of prime terms, whose four digits are permutations of each other.

prob49 =
  [ (a, a+d, a+d+d) |
    a <- takeWhile (<10000) $ dropWhile (<1000) primes,
    let s = sort (show a),
    d <- [1..(9999-a)`div`2],
    let (b,c) = (a+d, a+d+d),
    p!b && p!c,
    sort (show b) == s,
    sort (show c) == s]
  where p = funArray (1000,9999) is_prime
-- [(1487,4817,8147),(2969,6299,9629)]

main :: IO String
main = return $ show a ++ show b ++ show c 
  where
    (a,b,c) = last prob49
-- 296962999629

