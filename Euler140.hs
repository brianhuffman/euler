module Euler140 where
import EulerLib
import qualified SortedList as S

------------------------------------------------------------------------------
-- 140. Investigating the value of infinite polynomial series for which the coefficients are a linear second order recurrence relation.
{-
Consider the infinite polynomial series
AG(x) = xG(1) + x^2G(2) + x^3G(3) + ...,
where G(k) is the kth term of the second order recurrence relation
G(k) = G(k-1) + G(k-2), G(1) = 1 and G(2) = 4;
that is, 1, 4, 5, 9, 14, 23, ... .

For this problem we shall be concerned with values of x for which AG(x) is a
positive integer.

The corresponding values of x for the first five natural numbers are shown
below.
x              AG(x)
(sqrt(5)-1)/4	 1
2/5              2
(sqrt(22)-2)/6   3
(sqrt(137)-5)/14 4
1/2              5

We shall call AG(x) a golden nugget if x is rational, because they become
increasingly rarer; for example, the 20th golden nugget is 211345365.

Find the sum of the first thirty golden nuggets.
-}

{-
See also Problem 137.

Analysis:
y = (SUM n>0. x^n * G(n))
y = x^1 * G(1) + (SUM n>1. x^n * G(n))
y = x + (SUM n>1. x^n * G(n))
y = x + x^2 * G(2) + (SUM n>2. x^n * G(n))
y = x + 4x^2 + (SUM n>2. x^n * G(n))
y = x + 4x^2 + (SUM n>0. x^(n+2) * G(n+2))
y = x + 4x^2 + (SUM n>0. x^(n+2) * (G(n+1) + G(n)))
y = x + 4x^2 + (SUM n>0. x^(n+2) * G(n+1) + x^(n+2) * G(n))
y = x + 4x^2 + (SUM n>0. x^(n+2) * G(n+1)) + (SUM n>0. x^(n+2) * G(n))
y = x + 4x^2 + (SUM n>0. x * x^(n+1) * G(n+1)) + (SUM n>0. x^2 * x^n * G(n))
y = x + 4x^2 + x * (SUM n>0. x^(n+1) * G(n+1)) + x^2 * (SUM n>0. x^n * G(n))
y = x + 4x^2 + x * (SUM n>1. x^n * G(n)) + x^2 * (SUM n>0. x^n * G(n))
y = x + 4x^2 + x * (y - x) + x^2 * y
y = x + 3*x^2 + x*y + x^2*y

Solving for y:
y - x*y - x^2*y = x + 3*x^2
y*(1 - x - x^2) = x + 3*x^2
y = (3*x^2 + x) / (1 - x - x^2)

Solving for x:
(y+3)*x^2 + (y+1)*x - y = 0

x = [ -(y+1) +/- sqrt [(y+1)^2 - 4(3+y)(-y)] ] / 2(3+y)
x = [ -(y+1) +/- sqrt [y^2 + 2y + 1 - 4(3+y)(-y)] ] / 2(3+y)
x = [ -(y+1) +/- sqrt [y^2 + 2y + 1 + 4(3+y)y] ] / 2(3+y)
x = [ -(y+1) +/- sqrt [y^2 + 2y + 1 + 12y + 4y^2] ] / 2(3+y)
x = [ -(y+1) +/- sqrt [5y^2 + 14y + 1] ] / 2(3+y)

For x to be rational, (5*y^2 + 14*y + 1) must be a perfect square.

Integer solutions to 5*y^2 + 14*y + 1 = n^2
(y, n)
------
(2, 7)
(5, 14)
(21, 50)
(42, 97)
(152, 343)
(296, 665)

Recurrence equations:
y' = (-429*y + 193*n - 473) / 4
n' = (-967*y + 435*n - 1055) / 4


(1050, 2351)
(2037, 4558)
(7205, 16114)
(13970, 31241)
...

y' = -9*y - 4*n - 14
n' = -20*y - 9*n - 28
This recurrence relation from http://www.alpertron.com.ar/QUAD.HTM
-}

find_solutions :: [Integer]
find_solutions =
  S.intersect
    [ 5*y^2 + 14*y + 1 | y <- [1 ..] ]
    [ n^2 | n <- [1 ..] ]

-- this is wrong!
--golden_nuggets :: [Integer]
golden_nuggets = f 2 7
  where
    f y n = (y,n) : f y' n'
      where
        y' = (-429*y + 193*n - 473) `div` 4
        n' = (-967*y + 435*n - 1055) `div` 4

prob140b = (2,7):(5,14):(21,50):(42,97):(152,343):(296,665):(map (f . f) prob140b)
  where f (y,n) = (-9*y - 4*n - 14, -20*y - 9*n - 28)

prob140 = sum $ take 30 $ map fst prob140b
-- 5673835352990

main :: IO String
main = return $ show $ prob140

linear_recurrence (a1,b1) (a2,b2) (a3,b3) (a4,b4) = (f a2 a3 a4, f b2 b3 b4)
  where
    f i j k = (x,y,z)
      where
        z1 = y2*(a1*k - a3*i) - (a1*b3 - a3*b1)*(a1*j - a2*i)
        z2 = y2*(a1 - a3) - (a1*b3 - a3*b1)*(a1 - a2)
        z = z1 / z2
        y1 = a1*j - a2*i - (a1 - a2)*z
        y2 = a1*b2 - a2*b1
        y = y1 / y2
        x = (i - b1*y - z) / a1

