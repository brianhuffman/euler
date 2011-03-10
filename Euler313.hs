module Euler313 where
import Primes

{-

Problem 313
05 December 2010

In a sliding game a counter may slide horizontally or vertically into
an empty space. The objective of the game is to move the red counter
from the top left corner of a grid to the bottom right corner; the
space always starts in the bottom right corner. For example, the
following sequence of pictures show how the game can be completed in
five moves on a 2 by 2 grid.

Let S(m,n) represent the minimum number of moves to complete the game
on an m by n grid. For example, it can be verified that S(5,4) = 25.

There are exactly 5482 grids for which S(m,n) = p^2, where p < 100 is
prime.

How many grids does S(m,n) = p^2, where p < 10^6 is prime?

-}


{-

 S |  2   3   4   5   6   7   8
-------------------------------
 2 |  5   9  15  21  27  33  39
 3 |  9  13  17  23  29  35  41
 4 | 15  17  21  25  31  37  43  49
 5 | 21  23  25  29  33  39  45
 6 | 27  29  31  33  37  41  47
 7 | 33  35  37  39  41  45  49
 8 | 39  41  43  45  47  49  53

S(m,n) | (m < n) = 2m + 6n - 13
       | (n < m) = 2n + 6m - 13
       | (m = n) = 8n - 11

S(n,n) == 3 (mod 8)
3 is not a QR (mod 8), so S(n,n) is never a square.

S(n, n+1) = 8n - 7 == 1 (mod 8), is a QR.
S(n, n+2) = 8n - 1 == 7 (mod 8), not a QR.
S(n, n+3) = 8n + 5 == 5 (mod 8), not a QR.
S(n, n+4) = 8n + 11 == 3 (mod 8), not a QR.
S(n, n+5) = S(n+3, n+4).

---------------------------------------

There are 78498 primes below 1 million.

-}

type Z = Integer

grids :: Z -> [(Z, Z)]
grids p = f (n, n+1)
  where
    n = (p^2 + 7) `div` 8
    f (a, b) = if a>1 then (a, b) : f (a-3, b+1) else []

num_grids :: Z -> Z
num_grids p = ((n+1) `div` 3) * 2
  where n = (p^2 + 7) `div` 8

prob313 :: Z -> Z
prob313 m = sum (map num_grids (takeWhile (<m) primes))

main :: IO String
main = return $ show $ prob313 (10^6)

answer :: String
answer = "2057774861813004"
