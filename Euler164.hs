module Euler164 where
import EulerLib
import Data.Array

{-
Problem 164

20 October 2007

How many 20 digit numbers n (without any leading zero) exist such that no
three consecutive digits of n have a sum greater than 9?
-}

-- count m ! (x,y,n) =
-- how many (n+2) digit numbers, with first digits x and y,
-- have no three consecutive digits with a sum greater than 9
count m = a
  where
    a = funArray ((0,0,0),(9,9,m-2)) f
    f (x,y,0) = if x+y <= 9 then 1 else 0
    f (x,y,n) = sum [ a!(y,z,n-1) | z <- [0..9-x-y] ]

prob164 :: Int -> Integer
prob164 m =
  sum [ count m ! (x,y,m-2) | x <- [1..9], y <- [0..9] ]

main :: IO String
main = return $ show $ prob164 20
-- 378158756814587
