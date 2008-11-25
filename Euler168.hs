module Euler168 where

{-
Problem 168
Number Rotations

16 November 2007

Consider the number 142857. We can right-rotate this number by moving the
last digit (7) to the front of it, giving us 714285.

It can be verified that 714285 = 5*142857.

This demonstrates an unusual property of 142857: it is a divisor of its
right-rotation.

Find the last 5 digits of the sum of all integers n, 10 < n < 10^100, that
have this property.
-}

-- give a string of digits, starting with d0,
-- representing a number which when multiplied by k
-- returns the right-rotation of itself
rr_divisor d0 k = f d0
  where
    f d = let (q, r) = divMod d 10
              d' = r*k + q
          in r : if d' == d0 then [] else f d'

-- convert list of digits to value
-- least significant digit first, e.g. decimal [1,2,3] = 321
decimal = foldr (\d x -> x*10 + d) 0

prob168 m n = sum
  [ decimal (take n ys) |
    k <- [1 .. 9],
    d0 <- [k .. 9],
    let xs = rr_divisor d0 k,
    l <- [1 .. m `div` length xs],
    let ys = concat (replicate l xs),
    not (null (tail ys)),
    let x = decimal (take 5 ys) ]
  `mod` 10^n

main :: IO String
main = return $ show $ prob168 100 5
-- 59206
