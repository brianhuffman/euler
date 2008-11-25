module Euler145 where
import EulerLib
import Char (digitToInt)

------------------------------------------------------------------------------
-- 145. How many reversible numbers are there below one-billion?
{-
Some positive integers n have the property that the sum [ n + reverse(n) ]
consists entirely of odd (decimal) digits. For instance, 36 + 63 = 99 and
409 + 904 = 1313. We will call such numbers reversible; so 36, 63, 409, and
904 are reversible. Leading zeroes are not allowed in either n or reverse(n).

There are 120 reversible numbers below one-thousand.

How many reversible numbers are there below one-billion (109)?
-}

{-
1 digit: 0
2 digit: 20
3 digit: 100
4 digit: 600
5 digit: 0
6 digit: 18000
7 digit: 50000
8 digit: 540000
9 digit: 0

Even lengths:

  6 5 4 3 2 1
-------------
  a b c d e f
+ f e d c b a
-------------

Rules:
- column 1 sums odd.
- column a sums odd <--> column (n'-a) sums odd
- column a carries  <--> column (n'-a) carries
- column a sums odd <--> column (a-1) doesn't carry

--------------------------------------------------------
for 6 digits (1-6):
[1,6] odd, [2,5] n/c, [3,4] odd, [3,4] n/c, [2,5] odd, [1,6] n/c.

for 8 digits (1-8):
[1,8] odd, [2,7] n/c, [3,6] odd, [4,5] n/c,
[4,5] odd, [3,6] n/c, [2,7] odd, [1,8] n/c.

-- for even lengths, every pair must sum to 1,3,5,7, or 9
30 possible pairs for (a+b) in [1,3,5,7,9].
20 possible pairs for (a+b) in [1,3,5,7,9] with a,b /= 0.

length 2n --> 20 * 30^(n-1)

--------------------------------------------------------
for 9 digits (1-9):
[1,9] odd, [2,8] n/c, [3,7] odd, [4,6] n/c, [5,5] odd.
fail!

length 4n+1 --> 0

--------------------------------------------------------
for 3 digits (1-3):
[1,3] odd, [2,2] n/c.
[2,2] even, [1,3] c.

for 7 digits (1-7):
[1,7] odd, [2,6] n/c, [3,5] odd, [4,4] n/c.
[4,4] even, [3,5] c, [2,6] even, [1,7] c.

20 possible pairs for (a+b) in [11,13,15,17] with a,b /= 0.
25 possible pairs for (a+b) in [0,2,4,6,8].
5 possible pairs for (a+a) in [0,2,4,6,8].

length 4n+3 --> 100 * 500^n
-}

reversible :: Integer -> Bool
reversible n =
  not (divides 10 n) &&
  (all (odd . digitToInt) $ show (n + read (reverse (show n))))

-- reversibles_len n = length (filter reversible [10^(n-1) .. 10^n-1])
reversibles_len :: Int -> Integer
reversibles_len n =
  case divMod n 4 of
    (q,0) -> 20 * 30^(2*q-1)
    (q,1) -> 0
    (q,2) -> 20 * 900^q
    (q,3) -> 100 * 500^q

-- reversibles_upto n = length (filter reversible [1 .. 10^n-1])
reversibles_upto :: Int -> Integer
reversibles_upto n = sum $ map reversibles_len [1 .. n]

main :: IO String
main = return $ show $ reversibles_upto 9
-- 608720
