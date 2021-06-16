module Euler255 where
import Data.Ratio
import EulerLib (showFloat)

{-
Problem 255
Rounded Square Roots

11 September 2009

We define the rounded-square-root of a positive integer n as the
square root of n rounded to the nearest integer.

The following procedure (essentially Heron's method adapted to integer
arithmetic) finds the rounded-square-root of n:

Let d be the number of digits of the number n.
If d is odd, set x_(0) = 2×10^((d-1)/2).
If d is even, set x_(0) = 7×10^((d-2)/2).
Repeat:

until x_(k+1) = x_(k).

As an example, let us find the rounded-square-root of n = 4321.
n has 4 digits, so x_(0) = 7×10^((4-2)/2) = 70.

Since x_(2) = x_(1), we stop here.

So, after just two iterations, we have found that the
rounded-square-root of 4321 is 66 (the actual square root is
65.7343137…).

The number of iterations required when using this method is
surprisingly low.  For example, we can find the rounded-square-root of
a 5-digit integer (10,000 ≤ n ≤ 99,999) with an average of
3.2102888889 iterations (the average value was rounded to 10 decimal
places).

Using the procedure described above, what is the average number of
iterations required to find the rounded-square-root of a 14-digit
number (10^(13) ≤ n < 10^(14))?

Give your answer rounded to 10 decimal places.

Note: The symbols ⌊x⌋ and ⌈x⌉ represent the floor function and ceiling
function respectively.

-}

type Z = Integer

next :: Z -> Z -> Z
next n x = y
  where d = (n + x - 1) `div` x  -- ceiling (n / x)
        y = (x + d) `div` 2  -- floor ((x + d) / 2)

-- least n such that next n x == y
next_inv :: Z -> Z -> Z
next_inv x y = n
  where d = 2*y - x
        n = (d-1) * x + 1

iters :: Z -> Z -> [(Z, Z)]
iters n x
  | x == x'   = (x, x') : []
  | otherwise = (x, x') : iters n x'
  where x' = next n x

-- 5 digit integer: x0 = 200
-- 4 digit integer: x0 = 70

foo x0 n = (l, n')
  where
    its = iters n x0
    l = length its
    n' = minimum $ map (\(x, y) -> next_inv x (y+1)) its

foos x0 n = (l, n') : foos x0 n'
  where (l, n') = foo x0 n

{-
foo_total x0 n nmax
  | n' > nmax = fromIntegral l * (nmax + 1 - n)
  | otherwise = fromIntegral l * (n' - n) + foo_total x0 n' nmax
  where (l, n') = foo x0 n
-}

foo_total x0 n0 nmax = helper 0 n0
  where
    helper t n
      | n' > nmax = t + fromIntegral l * (nmax + 1 - n)
      | otherwise = helper (t + fromIntegral l * (n' - n)) n'
      where (l, n') = foo x0 n

{-
3 digits: avg = 2362 / 900 ~~ 2.6244444444444444
4 digits: avg = 25545 / 9000 ~~ 2.8383333333333334
5 digits: avg = 288926 / 90000 ~~ 3.210288888888889
6 digits: avg = 3008866 / 900000 ~~ 3.3431844444444443



5 digits: 10000-99999
square roots range from 100 to 316


next n x = y
next n' x = y'

next n x = floor[(x + ceil[n / x]) / 2]


"floor[(x + ceil[n / x]) / 2]" can only increment when "ceil[n / x]"
increments.

This happens when "n" goes from a multiple of "x" to one more than a
multiple of "x".

Prelude Euler255> foo_total 700 100000 999999
3008866
Prelude Euler255> foo_total 7000 10000000 99999999
333790803
Prelude Euler255> foo_total 70000 1000000000 9999999999
35510925913
Prelude Euler255> foo_total 700000 100000000000 999999999999
3783445941464
Prelude Euler255> foo_total 7000000 10000000000000 99999999999999
400266100622279

-}

main :: IO String
main = return $ showFloat 10 (total % (9*10^13))
  where total = foo_total (7*10^6) (10^13) (10^14-1)

answer :: String
answer = "4.4474011180"

{-

n = 10000:
200 -> 125  (until 10201)
125 -> 102  (until 10001)
102 -> 100  (until 10099)
100 -> 100  (until 10101)

n = 10001:
200 -> 125  (until 10201)
125 -> 103  (until 10251)
103 -> 100  (until 10095)
100 -> 100  (until 10101)

n = 10095:
200 -> 125  (until 10201)
125 -> 103  (until 10251)
103 -> 101  (until 10301)
101 -> 100  (until 10101)
100 -> 100  (until 10101)

n = 10101:
200 -> 125  (until 10201)
125 -> 103  (until 10251)
103 -> 101  (until 10301)
101 -> 101  (until 10303)


-}
