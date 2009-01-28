module Euler135 where
import Data.Array.Unboxed

{-
Problem 135
29 December 2006

Given the positive integers, x, y, and z, are consecutive terms of an
arithmetic progression, the least value of the positive integer, n,
for which the equation, x^(2) - y^(2) - z^(2) = n, has exactly two
solutions is n = 27:

34^(2) - 27^(2) - 20^(2) = 12^(2) - 9^(2) - 6^(2) = 27

It turns out that n = 1155 is the least value which has exactly ten
solutions.

How many values of n less than one million have exactly ten distinct
solutions?
-}

{-
We are given 0 < z < y < x where x, y, z
are a descending arithmetic progression.

Then define the difference d = x-y = y-z.
x = y + d
z = y - d
0 < d < y

Now let's simplify the original equation:
n = x^2 - y^2 - z^2
  = (y+d)^2 - y^2 - (y-d)^2
  = (y^2 + 2dy + d^2) - y^2 - (y^2 - 2dy + d^2)
  = y^2 + 2dy + d^2 - y^2 - y^2 + 2dy - d^2 
  = 4dy - y^2
  = (4d - y)y

Next let's find some constraints on d and y to limit our search:
       4dy - y^2 < n
4d^2 - 4dy + y^2 > 4d^2 - n
      (2d - y)^2 > 4d^2 - n
        |2d - y| > square_root (4d^2 - n)

Laws about integer square_root function:
square_root (x^2) == x
(square_root x)^2 <= x
square_root x < square_root y ==> x < y

More constraints due to the fact that n is positive:
0 < 4d - y
y < 4d
0 < d < y < 4d
y`div`4 < d < y

(4d - y)y < nmax
(4d - y) <= nmax`div`y
4d <= nmax`div`y + y
d <= (nmax`div`y + y)`div`4

4dy - y^2 < nmax
4dy < nmax + y^2
d <= (nmax + y^2)`div`(4*y)

n = (4*d - y)*y < (4*y - y)*y = 3*y^2
Thus n < 3y^2.
 and y > sqrt (n/3)
-}

{-
(4d - y)*y = n
Thus y divides n.

Let q = 4*d - y.
Then n = q*y.
Also d = (q+y)/4

Since d < y, then q < 4*y - y = 3*y.
In terms of n,
  q < 3*y
  q^2 < 3*q*y
  q^2 < 3*n

(y + q) == 0  (mod 4)
(y,q) = (4a+0, 4b+0), n = 16ab
(y,q) = (4a+1, 4b+3), n = (4a+1)(4b+3) = 16ab + 12a + 4b + 3
(y,q) = (4a+2, 4b+2), n = (4a+2)(4b+2) = 16ab + 8a + 8b + 4
(y,q) = (4a+3, 4b+1), n = (4a+3)(4b+1) = 16ab + 4a + 12b + 3

So n must be equivalent to either 0 or 3 (mod 4).
But values of n == 8 (mod 16) are not possible.

Case 1:
  n == 0 (mod 16)
  y == 0 (mod 4)
  q == 0 (mod 4)

Case 2:
  n == 4 (mod 8)
  y == 2 (mod 4)
  q == 2 (mod 4)

Case 3:
  n == 3 (mod 4)
  y == 1 or 3 (mod 4)
  q == 3 or 1 (mod 4)
  Any factorization of n = q*y (with q < 3y) yields a solution.
-}

-- prob135a m = list of (n, multiplicity)
prob135a :: Int -> [(Int, Int)]
prob135a m = concatMap f [1 .. dmax]
  where
    dmax = m`div`4
    f d = takeWhile ((<m) . fst) $ g d ++ h d ++ [(4*d^2, 1)]
    g d = [ ((4*d - y)*y, 1) | y <- [1 .. d] ]
    h d = [ ((4*d - y)*y, 2) | y <- [d+1 .. 2*d-1] ]

prob135b :: Int -> UArray Int Int
prob135b m = accumArray (+) 0 (1,m) (prob135a m)

main :: IO String
main = return $ show $ length $ filter (==10) $ elems (prob135b (10^6))

answer :: String
answer = "4989"
