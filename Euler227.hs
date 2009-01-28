module Euler227 where

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

normalize :: [Rational] -> [Rational]
normalize (x:xs) = map (/x) xs

triangular :: [[Rational]] -> [[Rational]]
triangular [xs] = [1 : normalize xs]
triangular (xs:xss) = (1 : xs') : map (0:) (triangular (map reduce xss))
  where
    xs' = normalize xs
    reduce (k:ys) = zipWith (\x y -> y - k*x) xs' ys

solver :: [[Rational]] -> Rational
solver [[x,y]] = y / x
solver (xs:xss) = solver (map reduce xss)
  where
    xs' = normalize xs
    reduce (k:ys) = zipWith (\x y -> y - k*x) xs' ys

-- 10 player game.
xss10 :: [[Rational]]
xss10 =
  [ [-17,   8,   1,   0,   0, -36],
    [  8, -18,   8,   1,   0, -36],
    [  1,   8, -18,   8,   1, -36],
    [  0,   1,   8, -17,   8, -36],
    [  0,   0,   2,  16, -18, -36] ]


constraints :: Int -> [[Rational]]
constraints n = [ row k ++ [-1] | k <- [1 .. n] ]
  where
    row k
      | k == 1 = [ -17/36, 2/9, 1/36 ] ++ replicate (n-3) 0
      | k == 2 = [ 2/9, -1/2, 2/9, 1/36 ] ++ replicate (n-4) 0
      | k == n = replicate (n-3) 0 ++ [ 1/18, 4/9, -1/2 ]
      | k == n-1 = replicate (n-4) 0 ++ [ 1/36, 2/9, -17/36, 2/9 ]
      | otherwise =
          replicate (k-3) 0 ++
          [ 1/36, 2/9, -1/2, 2/9, 1/36 ] ++
          replicate (n-k-2) 0

show_significant :: RealFrac a => Int -> a -> String
show_significant d x = show_rounded (d - k) x
  where k = length (show (floor x))

show_rounded :: RealFrac a => Int -> a -> String
show_rounded d x =
  show (floor x) ++ "." ++ replicate (d - length (show r)) '0' ++ show r
  where
    r = round (x * 10^d) `mod` (10^d)

prob227 :: Int -> Rational
prob227 n = solver (constraints (n `div` 2))

main :: IO String
main = return $ show_significant 10 $ prob227 100

answer :: String
answer = "3780.618622"
