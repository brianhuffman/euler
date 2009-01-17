module Euler227 where
import Data.Array.Unboxed

{-

Problem 227
10 January 2009

"The Chase" is a game played with two dice and an even number of
players.

The players sit around a table; the game begins with two opposite
players having one die each. On each turn, the two players with a die
roll it.  If a player rolls a 1, he passes the die to his neighbour on
the left; if he rolls a 6, he passes the die to his neighbour on the
right; otherwise, he keeps the die for the next turn.  The game ends
when one player has both dice after they have been rolled and passed,
that player has then lost.

In a game with 100 players, what is the expected number of turns the
game lasts?

Give your answer rounded to ten significant digits.

-}



{-
Let E(x) =
  expected number of turns for game to last, 
  where dice are x spaces apart.

Each die:
  * 1/6 to move left (L)
  * 1/6 to move right (R)
  * 4/6 to stay put. (-)

Both dice
  * 1/36 : <- <-  (0)
  * 1/36 : <- ->  (+2)
  * 4/36 : <- --  (+1)
  * 1/36 : -> ->  (0)
  * 1/36 : -> <-  (-2)
  * 4/36 : -> --  (-1)
  * 4/36 : -- <-  (-1)
  * 4/36 : -- ->  (+1)
  * 16/36: -- --  (0)

Transition probabilities:
 +2: 1/36
 +1: 8/36
 =0: 18/36
 -1: 8/36
 -2: 1/36

E(0) = 0
E(x) = 1 + [ E(x-2) + 8 E(x-1) + 18 E(x) + 8 E(x+1) + E(x+2) ] / 36

E(x) = 1 + [ E(x-2) + 8 E(x-1) + 8 E(x+1) + E(x+2) ] / 36 + E(x)/2
E(x) = 1 + [ E(x-2) + 8 E(x-1) + 8 E(x+1) + E(x+2) ] / 36 + E(x)/2
E(x)/2 = 1 + [ E(x-2) + 8 E(x-1) + 8 E(x+1) + E(x+2) ] / 36
E(x) = 2 + [ E(x-2) + 8 E(x-1) + 8 E(x+1) + E(x+2) ] / 18
-}

n :: Int
n = 50

type State = UArray Int Double

initial :: State
initial = listArray (0, n) (repeat 0)
--initial = listArray (0, 2*n-1) (repeat 0)

next :: State -> State
next a = listArray (0, n) (map f [0 .. n])
  where
{-
    f i
      | i == 0    = 0
      | i == 1    = 1 + e(1)/36 + e(1)/2 + e(2)*2/9 + e(3)/36
      | i == n-1  = 1 + e(n-3)/36 + e(n-2)*2/9 + e(n-1)/2 + e(n)*2/9 + e(n-1)/36
      | i == n    = 1 + e(n-2)/18 + e(n-1)*4/9 + e(n)/2
      | otherwise = 1 + e(i-2)/36 + e(i-1)*2/9 + e(i)/2 + e(i+1)*2/9 + e(i+2)/36
-}
    f 0 = 0
    f i = 1 + e(i-2)/36 + e(i-1)*2/9 + e(i)/2 + e(i+1)*2/9 + e(i+2)/36
--    f i = 2 + e(i-2)/18 + e(i-1)*4/9 + e(i+1)*4/9 + e(i+2)/18
--    e j = a!j
    e j
      | j < 0 = a!(-j)
      | j > n = a!(2*n-j)
      | otherwise = a!j
{-
next :: State -> State
next a = listArray (0, 2*n-1) (map f [0 .. 2*n-1])
  where
    f 0 = 0
    f i = 1 + (e(i-2) + 8*e(i-1) + 18*e(i) + 8*e(i+1) + e(i+2)) / 36
    e j = a!(j `mod` (2*n))
-}

states :: [State]
states = iterate next initial

state :: Int -> State
state k = iterate next initial !! k

fixed_state :: State
fixed_state = fix initial
  where
    fix s = if s == s' then s else fix s'
      where s' = next s

-- 10: 156.12372435560127
-- 20: 612.2474487138957
-- 50: 3780.6186217842583

-- 50: 3780.6186217846885