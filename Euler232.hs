module Euler232 where
import EulerLib (funArray, showFloat)
import Data.Array

{-
Problem 232
13 February 2009

Two players share an unbiased coin and take it in turns to play "The
Race". On Player 1's turn, he tosses the coin once: if it comes up
Heads, he scores one point; if it comes up Tails, he scores
nothing. On Player 2's turn, she chooses a positive integer T and
tosses the coin T times: if it comes up all Heads, she scores 2^(T-1)
points; otherwise, she scores nothing. Player 1 goes first. The winner
is the first to 100 or more points.

On each turn Player 2 selects the number, T, of coins that maximises
the probability of her winning.

What is the probability that Player 2 wins?

Give your answer rounded to eight decimal places in the form
0.abcdefgh .
-}

{-
Player 2 can toss:
1 time for 1 point
2 times for 2 points
3 times for 4 points
4 times for 8 points
...

P1 will succeed with probability 1/2.
P1 will fail with probability 1/2.

P2 will succeed with probability 1/(2^T).
P2 will fail with probability (2^T - 1)/(2^T).

On P2's turn, P2 has a probability of 2/(2^T + 1)
of succeeding before P1 does.

On P2's turn, P2 has a probability of 1/(2^T + 1)
of succeeding before P1 does, followed by P2 failing.

On P2's turn, P2 has a probability of 1/(2^T + 1)
of succeeding before P1 does, followed by P2 succeeding.

On P2's turn, P2 has a probability of (2^T - 1)/(2^T + 1)
of P1 succeeding before she does.

Transition probabilities:
* Going for the win:
  (a,b) -> (a,   0):         2 / (2^T+1)
  (a,b) -> (a-1, b):   (2^T-1) / (2^T+1)
* Not going for the win:
  (a,b) -> (a,   b - 2^(T-1)):      1 / (2^T+1)
  (a,b) -> (a-1, b - 2^(T-1)):      1 / (2^T+1)
  (a,b) -> (a-1, b):          (2^T-1) / (2^T+1)
-}

-- type R = Rational
type R = Double

to_win :: Int -> Int
to_win 1 = 1
to_win n = 1 + to_win ((n+1)`div`2)

prob :: Int -> R
prob t = 2 / (2^t + 1)

prob' :: Int -> R
prob' t = 1 - prob t

race_array :: Int -> Array (Int, Int) (R, Int)
race_array m = a
  where
    a = funArray ((0,1),(m,m)) f
    f (0,j) = (0, 0)
    f (i,j) = maximum
      [ g (i,j) t |
        t <- [1 .. to_win j] ]
    g (i,j) t = (prob t * win + prob' t * lose, t)
      where
        d = 2^(t-1)
        win = if d < j then (fst (a!(i,j-d)) + fst (a!(i-1,j-d)))/2 else 1
        lose = if 1 < i then fst (a!(i-1,j)) else 0

prob232 :: Int -> R
prob232 m = (fst (a!(m,m)) + fst (a!(m-1,m))) / 2
  where a = race_array m

main :: IO String
main = return $ showFloat 8 $ prob232 100

answer :: String
answer = "0.83648556"

{-

[1,2,3,3,4,4,4,4,5,5,5,5,5,5,5,5,6,6,6,6]
[1,1,2,3,3,4,4,4,4,5,5,5,5,5,5,5,5,5,6,6]
[1,1,1,2,3,3,4,4,4,4,5,5,5,5,5,5,5,5,5,6]
[1,1,1,2,1,3,3,4,4,4,4,4,5,5,5,5,5,5,5,5]
[1,1,1,1,1,2,3,3,4,4,4,4,5,5,5,5,5,5,5,5]
[1,1,1,1,1,2,3,3,1,4,4,4,4,5,5,5,5,5,5,5]
[1,1,1,1,1,1,2,3,1,4,4,4,4,4,5,5,5,5,5,5]
[1,1,1,1,1,1,1,2,1,3,4,4,4,4,4,5,5,5,5,5]
[1,1,1,1,1,1,1,2,1,3,3,4,4,4,4,4,5,5,5,5]
[1,1,1,1,1,1,1,1,1,2,3,3,4,4,4,4,4,5,5,5]
[1,1,1,1,1,1,1,1,1,1,2,3,3,4,4,4,4,4,5,5]
[1,1,1,1,1,1,1,1,1,1,1,2,3,3,4,4,4,4,4,5]
[1,1,1,1,1,1,1,1,1,1,1,2,3,3,3,4,4,4,4,4]
[1,1,1,1,1,1,1,1,1,1,1,1,2,3,3,4,4,4,4,4]
[1,1,1,1,1,1,1,1,1,1,1,1,1,2,3,3,1,4,4,4]
[1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,3,1,2,4,4]
[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,3,3,4]
[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,2,3,3]
[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3]
[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,3]
-}
