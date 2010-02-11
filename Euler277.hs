module Euler277 where
import Data.Ratio
import Data.Char

{---------------------------------------------------------------------
Problem 277
A Modified Collatz Sequence

06 February 2010

A modified Collatz sequence of integers is obtained from a starting
value a_(1) in the following way:

a_(n+1) = a_(n)/3 if a_(n) is divisible by 3. We shall denote this as
a large downward step, "D".

a_(n+1) = (4a_(n) + 2)/3 if a_(n) divided by 3 gives a remainder of
1. We shall denote this as an upward step, "U".

a_(n+1) = (2a_(n) - 1)/3 if a_(n) divided by 3 gives a remainder of
2. We shall denote this as a small downward step, "d".

The sequence terminates when some a_(n) = 1.

Given any integer, we can list out the sequence of steps.  For
instance if a_(1)=231, then the sequence
{a_(n)}={231,77,51,17,11,7,10,14,9,3,1} corresponds to the steps
"DdDddUUdDD".

Of course, there are other sequences that begin with that same
sequence "DdDddUUdDD....".  For instance, if a_(1)=1004064, then the
sequence is DdDddUUdDDDdUDUUUdDdUUDDDUdDD.  In fact, 1004064 is the
smallest possible a_(1) > 10^(6) that begins with the sequence
DdDddUUdDD.

What is the smallest a_(1) > 10^(15) that begins with the sequence
"UDDDUdddDDUDDddDdDddDDUDDdUUDd"?

---------------------------------------------------------------------}

base3 :: Integer -> String -> Integer
base3 n cs = foldl dig3 n cs
  where
    dig3 n c = 3*n + fromIntegral (digitToInt c)

try cs = take (length cs) (str (base3 1 cs))
b3 = "110122202101110111122121021111"
trial_and_error = try b3

str :: Integer -> String
str n = case divMod n 3 of
  (0, 1) -> []
  (q, 0) -> 'D' : str q
  (q, 1) -> 'U' : str (4*q+2)
  (q, 2) -> 'd' : str (2*q+1)

s1 = "DdDddUUdDDDdUDUUUdDdUUDDDUdDD"

test = str 1004064 == s1

type R = Rational

step :: (R, R) -> Char -> (R, R)
step (x, y) 'D' = (  x/3,       y/3)
step (x, y) 'U' = (4*x/3, (4*y+2)/3)
step (x, y) 'd' = (2*x/3, (2*y-1)/3)

steps :: String -> (R, R)
steps = foldl step (1, 0)

s2 = "UDDDUdddDDUDDddDdDddDDUDDdUUDd"

main :: IO String
main = return $ show $ base3 5 b3

answer :: String
answer = "1125977393124310"

{---------------------------------------------------------------------

D: n == 0 (mod 3): n -> n/3
U: n == 1 (mod 3): n -> (4n + 2)/3
d: n == 2 (mod 3): n -> (2n - 1)/3

(3n+0) -> n
(3n+1) -> 4n+2
(3n+2) -> 2n-1

n0 = a
U: a == 1, a = 3b+1
n0 = 3b+1
n1 = 4b+2
D: 4b+2 == 0, 4b == 1, b == 1, b = 3c+1, a = 3(3c+1)+1 = (3
n1 = 4(3c+1)+2 = 12c+6
n2 = 4c+2



---------------------------------------------------------------------}

