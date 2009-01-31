module Euler230 where

{-
Problem 230
Fibonacci Words

31 January 2009

For any two strings of digits, A and B, we define F_(A,B) to be the
sequence (A,B,AB,BAB,ABBAB,...) in which each term is the
concatenation of the previous two.

Further, we define D_(A,B)(n) to be the n^(th) digit in the first term
of F_(A,B) that contains at least n digits.

Example:

Let A=1415926535, B=8979323846. We wish to find D_(A,B)(35), say.

The first few terms of F_(A,B) are:
1415926535
8979323846
14159265358979323846
897932384614159265358979323846
14159265358979323846897932384614159265358979323846

Then D_(A,B)(35) is the 35^(th) digit in the fifth term, which is 9.

Now we use for A the first 100 digits of π behind the decimal point:
14159265358979323846264338327950288419716939937510
58209749445923078164062862089986280348253421170679

and for B the next hundred digits:
82148086513282306647093844609550582231725359408128
48111745028410270193852110555964462294895493038196 .

Find Σ_(n=0,1,...,17)( D_(A,B)((127+19n)×7^(n)) × 10^(n) ).
-}

stringfib a b = a : stringfib b (a ++ b)
-- stringfib a b !! n = stringfib a b !! (n-2) ++ stringfib a b !! (n-1)

functionD :: [a] -> [a] -> Integer -> a
functionD a b n0 = nth i0 (n0-1)
  where
    la = fromIntegral (length a)
    lb = fromIntegral (length b)
    ls = fibs la lb
    fibs x y = x : fibs y (x + y)
    i0 = length (takeWhile (<= n0) ls)
    -- nth i n = stringfib a b !! i !! n
    nth 0 n = a !! fromIntegral n
    nth 1 n = b !! fromIntegral n
    nth i n = if n < l then nth (i-2) n else nth (i-1) (n-l)
      where l = ls !! (i-2)

testA = "1415926535"
testB = "8979323846"

stringA :: String
stringA = 
  "14159265358979323846264338327950288419716939937510" ++
  "58209749445923078164062862089986280348253421170679"

stringB :: String
stringB = 
  "82148086513282306647093844609550582231725359408128" ++
  "48111745028410270193852110555964462294895493038196"

indices :: [Integer]
indices = [ (127 + 19*n)*7^(n) | n <- [0 .. 17] ]

solution :: String
solution = map (functionD stringA stringB) (reverse indices)

main :: IO String
main = return solution

answer :: String
answer = "850481152593119296"
