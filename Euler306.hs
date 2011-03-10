module Euler306 where
import Data.Bits
import Data.List (sort)
import EulerLib
import Data.Array
import qualified Data.IntSet as S

{-

Problem 306
17 October 2010

The following game is a classic example of Combinatorial Game Theory:

Two players start with a strip of n white squares and they take
alternate turns. On each turn, a player picks two contiguous white
squares and paints them black. The first player who cannot make a move
loses.

    * If n = 1, there are no valid moves, so the first player loses
      automatically.
    * If n = 2, there is only one valid move, after which the second
      player loses.
    * If n = 3, there are two valid moves, but both leave a situation
      where the second player loses.
    * If n = 4, there are three valid moves for the first player; she
      can win the game by painting the two middle squares.
    * If n = 5, there are four valid moves for the first player (shown
      below in red); but no matter what she does, the second player
      (blue) wins.

So, for 1 ≤ n ≤ 5, there are 3 values of n for which the first player
can force a win. Similarly, for 1 ≤ n ≤ 50, there are 40 values of n
for which the first player can force a win.

For 1 ≤ n ≤ 1 000 000, how many values of n are there for which the
first player can force a win?

-}

{-

Assign a nim-value v(n) to each size of game.

Take xor of subgames, and mex over all possible choices.

v(0) = 0
v(1) = 0
v(2) = 1
v(3) = 1
v(4) = 2
v(5) = 0
v(6) = 3
v(7) = 1
...

-}

mex :: [Int] -> Int
mex xs = f 0 (S.toAscList (S.fromList xs))
  where
    f i [] = i
    f i (n : ns) = if i < n then i else f (i+1) ns

table :: Int -> Array Int Int
table n = a
  where
    a = funArray (0, n) f
    f k = mex [ xor (a!i) (a!j) | i <- [0..(k-2)`div`2], let j = k-2-i ]

prob306_slow n = length (filter (/= 0) (elems (table n)))

{-

Table starting with v(1):

        5       9                       21      25      29
        |       |                       |       |       |
0,1,1,2,0,3,1,1,0,3,3,2,2,4,0,5,2,2,3,3,0,1,1,3,0,2,1,1,0,4,5,2,7,4,
0,1,1,2,0,3,1,1,0,3,3,2,2,4,4,5,5,2,3,3,0,1,1,3,0,2,1,1,0,4,5,3,7,4,
8,1,1,2,0,3,1,1,0,3,3,2,2,4,4,5,5,9,3,3,0,1,1,3,0,2,1,1,0,4,5,3,7,4,
8,1,1,2,0,3,1,1,0,3,3,2,2,4,4,5,5,9,3,3,0,1,1,3,0,2,1,1,0,4,5,3,7,4,
8,1,1,2,0,3,1,1,0,3,3,2,2,4,4,5,5,9,3,3,0,1,1,3,0,2,1,1,0,4,5,3,7,4,
-- continues to repeat with cycle length 34.

Losing states:
[1,5,9,15,21,25,29,35,39,43,55,59,63,73,77,89,93,97]

1, 15, 35, and all n == {5,9,21,25,29} (mod 34)

-}

prob306 n = n - (a + b + c + d + e + f + g + h)
  where
    a = if n >= 1 then 1 else 0
    b = if n >= 15 then 1 else 0
    c = if n >= 35 then 1 else 0
    d = (n + 34 - 5) `div` 34
    e = (n + 34 - 9) `div` 34
    f = (n + 34 - 21) `div` 34
    g = (n + 34 - 25) `div` 34
    h = (n + 34 - 29) `div` 34

main :: IO String
main = return $ show $ prob306 1000000

answer :: String
answer = "852938"
