module Euler285 where
import Primes
import EulerLib (showFloat)

{---------------------------------------------------------------------
Problem 285
03 April 2010

Albert chooses a positive integer k, then two real numbers a, b are
randomly chosen in the interval [0,1] with uniform distribution.  The
square root of the sum (k·a+1)^(2) + (k·b+1)^(2) is then computed and
rounded to the nearest integer. If the result is equal to k, he scores
k points; otherwise he scores nothing.

For example, if k = 6, a = 0.2 and b = 0.85, then
(k·a+1)^(2) + (k·b+1)^(2) = 42.05.  The square root of 42.05 is
6.484... and when rounded to the nearest integer, it becomes 6.  This
is equal to k, so he scores 6 points.

It can be shown that if he plays 10 turns with k = 1, k = 2, ...,
k = 10, the expected value of his total score, rounded to five decimal
places, is 10.20914.

If he plays 10^(5) turns with k = 1, k = 2, k = 3, ..., k = 10^(5),
what is the expected value of his total score, rounded to five decimal
places?

---------------------------------------------------------------------}

{---------------------------------------------------------------------

For k = 1: Output is a point in the square [1..2] x [1..2]
Score is for 0.5 <= r < 1.5

For k = 2: Output is a point in the square [1..3] x [1..3]
Score is for 1.5 <= r < 2.5

area(r) = Area of circle centered at (0,0) with radius r,
intersected with the half-planes x > 1 and y > 1.

---------------------------------------------------------------------}

type R = Double

area :: R -> R
area r = (pi/4 - theta) * (r^2) - x + 1
  where theta = asin (1/r)
        x = sqrt (r^2 - 1)

area' r = if r < sqrt 2 then 0 else area r

area2 :: R -> R
area2 k = area' (k+0.5) - area' (k-0.5)

expected :: R -> R
expected k = area2 k / k

prob285 :: R -> R
prob285 m = sum (map expected [1..m])

main :: IO String
main = return $ showFloat 5 $ prob285 (10^5)

answer :: String
answer = "157055.80999"
