module Euler137 where
import qualified SortedList as S

------------------------------------------------------------------------------
-- 137. Determining the value of infinite polynomial series for which the coefficients are Fibonacci numbers.
{-
Consider the infinite polynomial series
AF(x) = x*F(1) + x^2*F(2) + x^3*F(3) + ...,
where F(k) is the kth term in the Fibonacci sequence: 1, 1, 2, 3, 5, 8, ... ;
that is, F(k) = F(k-1) + F(k-2), F(1) = 1 and F(2) = 1.

For this problem we shall be interested in values of x for which AF(x) is a
positive integer.

Surprisingly
AF(1/2) = (1/2)*1 + (1/2)^2*1 + (1/2)^3*2 + (1/2)^4*3 + (1/2)^5*5 + ...
        = 1/2 + 1/4 + 2/8 + 3/16 + 5/32 + ...
        = 2

The corresponding values of x for the first five natural numbers are shown
below.
x               AF(x)
sqrt(2) - 1     1
1/2             2
(sqrt(13)-2)/3  3
(sqrt(89)-5)/8  4
(sqrt(34)-3)/5  5

We shall call AF(x) a golden nugget if x is rational, because they become
increasingly rarer; for example, the 10th golden nugget is 74049690.

Find the 15th golden nugget.
-}

{-
Analysis:
y = (SUM n>=0. x^n * F(n))
y = x^0 * F(0) + (SUM n>=1. x^n * F(n))
y = (SUM n>=1. x^n * F(n))
y = x^1 * F(1) + (SUM n>=2. x^n * F(n))
y = x + (SUM n>=2. x^n * F(n))
y = x + (SUM n>=0. x^(n+2) * F(n+2)))
y = x + (SUM n>=0. x^(n+2) * (F(n+1) + F(n)))
y = x + (SUM n>=0. x^(n+2) * F(n+1)) + (SUM n>=0. x^(n+2) * F(n))
y = x + (SUM n>=0. x * x^(n+1) * F(n+1)) + (SUM n>=0. x^2 * x^n * F(n))
y = x + x * (SUM n>=0. x^(n+1) * F(n+1)) + x^2 * (SUM n>=0. x^n * F(n))
y = x + x * (SUM n>=1. x^n * F(n)) + x^2 * (SUM n>=0. x^n * F(n))
y = x + x * y + x^2 * y

Solving for y:
y - x*y - x^2*y = x
y*(1 - x - x^2) = x
y = x / (1 - x - x^2)

Solving for x:
y*x^2 + (y+1)*x - y = 0

x = [ -(y+1) +/- sqrt [(y+1)^2 - 4y(-y)] ] / 2y
x = [ -y-1 +/- sqrt [(y+1)^2 + 4y^2] ] / 2y
x = [ -y-1 +/- sqrt [y^2 + 2y + 1 + 4y^2] ] / 2y
x = [ -y-1 +/- sqrt [5y^2 + 2y + 1] ] / 2y

For x to be rational, (5*y^2 + 2*y + 1) must be a perfect square.


Integer solutions to 5*y^2 + 2*y + 1 = n^2
(y, n)
------
(2, 5)
(15, 34)
(104, 233)
(714, 1597)

Recurrence equations:
y' = (7y + 3n + 1) / 2
n' = (15y + 7n + 3) / 2
These equations only work if y+n is odd.

Change of variable:
Let n = y + 2k + 1
2k = n - y - 1

5*y^2 + 2*y + 1 = (y+2*k+1)^2
5*y^2 + 2*y + 1 = (y + 2k + 1)^2
5*y^2 + 2*y + 1 = y^2 + 4k^2 + 4ky + 4k + 2y + 1
4*y^2 = 4k^2 + 4ky + 4k
y^2 = k^2 + ky + k

Integer solutions to y^2 = k^2 + k*y + k
(y, k)
------
(2, 1)
(15, 9)
(104, 64)
(714, 441)

Recurrence equations:
y' = 5y + 3k + 2
k' = 3y + 2k + 1
-}

find_solutions :: [Integer]
find_solutions = S.intersect (map (\y -> 5*y^2 +2*y + 1) [1..]) (map (^2) [1..])
-- [25,1156,54289,2550409,119814916,5628750625,264431464441,12422650078084,583600122205489,27416783093579881
-- ys = [2,15,104,714,4895,33552,229970,1576239,10803704,74049690
-- ns = [5,34,233,1597,10946,75025,514229,3524578,24157817,165580141

golden_nuggets :: [Integer]
golden_nuggets = f 2 1
  where f y k = y : f (5*y + 3*k + 2) (3*y + 2*k + 1)

main :: IO String
main = return $ show $ golden_nuggets !! (15-1)
