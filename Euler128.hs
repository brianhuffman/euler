module Euler128 where
import Primes
import qualified SortedList as S

{-
Problem 128
29 September 2006

A hexagonal tile with number 1 is surrounded by a ring of six
hexagonal tiles, starting at "12 o'clock" and numbering the tiles 2 to
7 in an anti-clockwise direction.

New rings are added in the same fashion, with the next rings being
numbered 8 to 19, 20 to 37, 38 to 61, and so on. The diagram below
shows the first three rings.

         20
      21    37
   22    08    36
23    09    19    35
   10    02    18
24    03    07    34
   11    01    17
25    04    06    33
   12    05    16
26    13    15    32
   27    14    31
      28    30
         29

By finding the difference between tile n and each its six neighbours
we shall define PD(n) to be the number of those differences which are
prime.

For example, working clockwise around tile 8 the differences are 12,
29, 11, 6, 1, and 13. So PD(8) = 3.

In the same way, the differences around tile 17 are 1, 17, 16, 1, 11,
and 10, hence PD(17) = 2.

It can be shown that the maximum value of PD(n) is 3.

If all of the tiles for which PD(n) = 3 are listed in ascending order
to form a sequence, the 10th tile would be 271.

Find the 2000th tile in this sequence.
-}

{-
Table of neighbor differences:

      up  nw  sw  dn  se  ne
    ------------------------
 1 |   1   2   3   4   5   6

 2 |   6   7   1  -1   5  17
 3 |   6   7   8   1  -2  -1
 4 |  -1   7   8   9   1  -3
 5 |  -4  -1   8   9  10   1
 6 |   1  -5  -1   9  10  11
 7 |  12  -5  -6  -1  10  11

 8 |  12  13   1  -6  11  29
 9 |  12  13   1  -6  -7  -1
10 |  12  13  14   1  -7  -1
11 |  -1  13  14   1  -7  -8
12 |  -1  13  14  15   1  -8
13 |  -9  -1  14  15   1  -8
14 |  -9  -1  14  15  16   1
15 |  -9 -10  -1  15  16   1
16 |   1 -10  -1  15  16  17
17 |   1 -10 -11  -1  16  17
18 |  18   1 -11  -1  16  17
19 |  18 -11 -17 -12  -1  17

layer n:
v1: -6(n-1), 6n-1, 6n, 6n+1, 12n+5
e1: -6(n-1), -6(n-1)-1, 6n, 6n+1
v2: -6(n-1)-1, 6n, 6n+1, 6n+2
e2: -6(n-1)-1, -6(n-1)-2, 6n+1, 6n+2
v3: -6(n-1)-2, 6n+1, 6n+2, 6n+3
e3: -6(n-1)-2, -6(n-1)-3, 6n+2, 6n+3
v4: -6(n-1)-3, 6n+2, 6n+3, 6n+4
e4: -6(n-1)-3, -6(n-1)-4, 6n+3, 6n+4
v5: -6(n-1)-4, 6n+3, 6n+4, 6n+5
e5: -6(n-1)-4, -6(n-1)-5, 6n+4, 6n+5
v6: -6(n-1)-5, 6n+4, 6n+5, 6n+6
e6: -6(n-1)-5, -6(n-1)-6, 6n+5, 6n+6
l6: -(12(n-1)+5), -(6n-1), -6(n-1)-6, 6n+5, 6n+6

Only the first or last value in each layer can have 3 prime differences.
layer n:
v1: 6n-1, 6n+1, 12n+5
l6: 12(n-1)+5, 6n-1, 6n+5

v1: 3*n*(n-1) + 2
l6: 3*n*(n+1) + 1
-}

-- TODO: rewrite to not use S.union

three_prime_diffs :: [Integer]
three_prime_diffs = 1 : S.union xs ys
  where
    xs = [ 3*n*(n-1) + 2 | n <- [1 ..], all is_prime [6*n-1, 6*n+1, 12*n+5] ]
    ys = [ 3*n*(n+1) + 1 | n <- [2 ..], all is_prime [6*n-1, 6*n+5, 12*n-7] ]

prob128 :: Int -> Integer
prob128 m = three_prime_diffs !! (m - 1)

main :: IO String
main = return $ show $ prob128 2000

answer :: String
answer = "14516824220"
