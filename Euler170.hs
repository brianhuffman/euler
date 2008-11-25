module Euler170 where
import Permutation
import Data.List
import Data.Int ( Int64 )
import Data.Array

{-
Problem 170
01 December 2007

Take the number 6 and multiply it by each of 1273 and 9854:

6 * 1273 = 7638
6 * 9854 = 59124

By concatenating these products we get the 1 to 9 pandigital 763859124. We
will call 763859124 the "concatenated product of 6 and (1273,9854)". Notice
too, that the concatenation of the input numbers, 612739854, is also 1 to 9
pandigital.

The same can be done for 0 to 9 pandigital numbers.

What is the largest 0 to 9 pandigital 10-digit concatenated product of an
integer with two or more other integers, such that the concatenation of the
input numbers is also a 0 to 9 pandigital 10-digit number?
-}

{-
Problem: find greatest (concat ys)
where ys = [ k*x | x <- xs ]
and concat (k:xs), concat ys are 0-9 pandigital.

Approach: search all possible ys, greatest first.
Number of permutations of 10 digits = factorial 10 = 3628800.
We have 3.6 million permutations to check.

Each permutation contains the digits [0 .. 9], whose sum is 45.
Thus the value of every permutation is a multiple of 9.

If k is not a multiple of 3, then neither is xs, so neither is ys.
This is a contradiction; thus, k must be a multiple of 3.

Upper bound on k: If k >= 100, then length (k*x) = length x + 2 or 3.
With xs containing two integers of total length 7, the total length
of ys will be at least 11 - too long.

If k >= 10, then length (k*x) = length x + 1 or 2.
With xs containing two integers of total length 8, the total length
of ys will be at least 10. This means that each individual product
must only add one digit.

If k < 10, then length (k*x) = length x + 0 or 1.
With xs containing two integers of total length 9, the total length
of ys can range from 9 to 11. This means that one of the two products
must add a single digit, and the other adds none.


-}

type Z = Int64

can_split :: Z -> String -> Bool
can_split k "" = False
can_split k ('0' : []) = False
can_split k ('0' : '0' : ds) = can_split k ('0' : ds)
can_split k ('0' : ds)
  | length (show (k * read ds)) <= length ds + 1 = True
can_split k (d : ds) = can_split k ds

concat_products :: [(Z, Z, Z)]
concat_products =
  [ (k, x, y) |
    s <- permutations "9876543210",
    let y = read s,
    k <- [3,6 .. 100],
    k `div` 11 /= 0,
    let (x, r) = divMod y k,
    r == 0,
    let s' = sort (show x ++ show k),
    s' == "00123456789" || (k < 10 && s' == "0123456789"),
    can_split k (show x) ]
-- 27 * 365080149 = 9857164023
-- 27 * 36508,149 = 985716,4023

prob170 :: [String]
prob170 =
  [ s | s <- permutations "9876543210",
    let y = read s,
    k <- [3,6 .. 100],
    k `div` 11 /= 0,
    let (x, r) = divMod y k,
    r == 0,
    let s' = sort (show x ++ show k),
    s' == "00123456789" || (k < 10 && s' == "0123456789"),
    can_split k (show x) ]

main :: IO String
main = return $ head $ prob170
-- 9857164023
