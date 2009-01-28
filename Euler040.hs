module Euler040 where
import Char (digitToInt)

{-
Problem 40
28 March 2003

An irrational decimal fraction is created by concatenating the
positive integers:

0.123456789101112131415161718192021...

It can be seen that the 12^(th) digit of the fractional part is 1.

If d_(n) represents the n^(th) digit of the fractional part, find the
value of the following expression.

d_(1) × d_(10) × d_(100) × d_(1000) × d_(10000) × d_(100000) ×
d_(1000000)

-}

digit :: Int -> Char
digit n = dig 1 10 n
  where
    dig l k n
      | n < l*k = let (q,r) = divMod n l in show q !! r
      | otherwise = dig (l+1) (k*10) (n+k)

main :: IO String
main = return $ show $ product $ map (digitToInt . digit) ns
  where
    d = concatMap show [0 ..]
    ns = [1,10,100,1000,10000,100000,1000000]

answer :: String
answer = "210"
