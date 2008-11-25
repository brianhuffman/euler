module Euler043 where
import Permutation
import EulerLib (divides)
import Primes

{-
Problem 43
Find the sum of all pandigital numbers with an unusual
  sub-string divisibility property.

09 May 2003

The number, 1406357289, is a 0 to 9 pandigital number because it is made up of
each of the digits 0 to 9 in some order, but it also has a rather interesting
sub-string divisibility property.

Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note
the following:
d2 d3 d4 = 406 is divisible by 2
d3 d4 d5 = 063 is divisible by 3
d4 d5 d6 = 635 is divisible by 5
d5 d6 d7 = 357 is divisible by 7
d6 d7 d8 = 572 is divisible by 11
d7 d8 d9 = 728 is divisible by 13
d8 d9 d10 = 289 is divisible by 17

Find the sum of all 0 to 9 pandigital numbers with this property.
-}

unusual :: [Int] -> Bool
unusual [d1,d2,d3,d4,d5,d6,d7,d8,d9,d10] = and [
  even d4,
  divides 3 (d3 + d4 + d5),
  d6 == 0 || d6 == 5,
  divides 7 (100*d5 + 10*d6 + d7),
  divides 11 (100*d6 + 10*d7 + d8),
  divides 13 (100*d7 + 10*d8 + d9),
  divides 17 (100*d8 + 10*d9 + d10)]

value_of :: [Int] -> Integer
value_of = foldl (\x d -> 10*x + toInteger d) 0

{-
unusual_values :: [Integer]
unusual_values =
  map value_of $
  filter unusual $
  drop (factorial 9) $ -- those starting with 0
  permutations [0 .. 9]
-}
-- [1406357289,1430952867,1460357289,4106357289,4130952867,4160357289]

unusual_values :: [Integer]
unusual_values =
  [ value_of [d1,d2,d3,d4,d5,d6,d7,d8,d9,d10] |
    let ds = [0 .. 9],
    (d6, ds) <- remove1 ds,
    d6 == 0 || d6 == 5,
    (d4, ds) <- remove1 ds,
    even d4,
    (d5, ds) <- remove1 ds,
    (d7, ds) <- remove1 ds,
    divides 7 (100*d5 + 10*d6 + d7),
    (d8, ds) <- remove1 ds,
    divides 11 (100*d6 + 10*d7 + d8),
    (d9, ds) <- remove1 ds,
    divides 13 (100*d7 + 10*d8 + d9),
    (d10, ds) <- remove1 ds,
    divides 17 (100*d8 + 10*d9 + d10),
    (d3, ds) <- remove1 ds,
    divides 3 (d3 + d4 + d5),
    (d1, ds) <- remove1 ds,
    d1 /= 0,
    d2 <- ds ]

main :: IO String
main = return $ show $ sum unusual_values
-- 16695334890
