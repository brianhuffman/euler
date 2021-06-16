module Euler084 where
import EulerLib
import Data.Array

------------------------------------------------------------------------------
-- 84. In the game, Monopoly, find the three most popular squares when using two 4-sided dice.
{-
In the game, Monopoly, the standard board is set up in the following way:
GO  A1  CC1 A2  T1  R1  B1  CH1 B2  B3  JAIL
H2                                      C1
T2                                      U1
H1                                      C2
CH3                                     C3
R4                                      R2
G3                                      D1
CC3                                     CC2
G2                                      D2
G1                                      D3
G2J F3 	U2  F2  F1  R3  E3  E2  CH2 E1  FP

A player starts on the GO square and adds the scores on two 6-sided dice to
determine the number of squares they advance in a clockwise direction. Without
any further rules we would expect to visit each square with equal probability:
2.5%. However, landing on G2J (Go To Jail), CC (community chest), and CH
(chance) changes this distribution.

In addition to G2J, and one card from each of CC and CH, that orders the player
to go to directly jail, if a player rolls three consecutive doubles, they do
not advance the result of their 3rd roll. Instead they proceed directly to jail.

At the beginning of the game, the CC and CH cards are shuffled. When a player
lands on CC or CH they take a card from the top of the respective pile and,
after following the instructions, it is returned to the bottom of the pile.
There are sixteen cards in each pile, but for the purpose of this problem we
are only concerned with cards that order a movement; any instruction not
concerned with movement will be ignored and the player will remain on the
CC/CH square.

    * Community Chest (2/16 cards):
         1. Advance to GO
         2. Go to JAIL
    * Chance (10/16 cards):
         1. Advance to GO
         2. Go to JAIL
         3. Go to C1
         4. Go to E3
         5. Go to H2
         6. Go to R1
         7. Go to next R (railway company)
         8. Go to next R
         9. Go to next U (utility company)
        10. Go back 3 squares.

The heart of this problem concerns the likelihood of visiting a particular
square. That is, the probability of finishing at that square after a roll. For
this reason it should be clear that, with the exception of G2J for which the
probability of finishing on it is zero, the CH squares will have the lowest
probabilities, as 5/8 request a movement to another square, and it is the final
square that the player finishes at on each roll that we are interested in. We
shall make no distinction between "Just Visiting" and being sent to JAIL, and
we shall also ignore the rule about requiring a double to "get out of jail",
assuming that they pay to get out on their next turn.

By starting at GO and numbering the squares sequentially from 00 to 39 we can
concatenate these two-digit numbers to produce strings that correspond with
sets of squares.

Statistically it can be shown that the three most popular squares, in order,
are JAIL (6.24%) = Square 10, E3 (3.18%) = Square 24, and GO (3.09%) = Square
00. So these three most popular squares can be listed with the six-digit modal
string: 102400.

If, instead of using two 6-sided dice, two 4-sided dice are used, find the
six-digit modal string.
-}

dice4 = zip [2..8] $ map (/16) [1,2,3,4,3,2,1]
dice6 = zip [2..12] $ map (/36) [1,2,3,4,5,6,5,4,3,2,1]

type Dist = Array Int Double

init_dist :: Dist
init_dist = funArray (0,39) (const 2.5)

next_dist :: Dist -> Dist
next_dist a = funArray (0,39) p
  where
    -- q n is probability of landing on space n with normal roll
    q n = sum [ x * a!((n-k)`mod`40) | (k,x) <- dice4 ]
    ch1 = q 7 / 16
    ch2 = q 22 / 16
    ch3 = q 36 / 16
    ch = ch1 + ch2 + ch3
    cc = (q 2 + q 17 + q 33 + ch3) / 16
    -- p n is probability of ending on space n, considering special rules
    p 0 = q 0 + cc + ch -- GO
    p 2 = q 2 * 14/16 -- Community Chest 1
    p 4 = q 4 + ch1 -- T1
    p 5 = q 5 + ch + 2*ch3 -- R1
    p 7 = q 7 * 6/16 -- Chance 1
    p 10 = q 10 + q 30 + cc + ch -- Jail
    p 11 = q 11 + ch -- C1
    p 12 = q 12 + ch3 + ch1 -- U1
    p 15 = q 15 + 2*ch1 -- R2
    p 17 = q 17 * 14/16 -- Community Chest 2
    p 19 = q 19 + ch2 -- D3
    p 22 = q 22 * 6/16 -- Chance 2
    p 24 = q 24 + ch -- E3
    p 25 = q 25 + 2*ch2 -- R3
    p 28 = q 28 + ch2 -- U2
    p 30 = 0 -- Go To Jail
    p 33 = (q 33 + ch3) * 14/16 -- Community Chest 3
    p 36 = q 36 * 6/16 -- Chance 3
    p 39 = q 39 + ch -- H2
    p n = q n

show_dist :: Dist -> String
show_dist =
  concatMap show . take 3 . reverse . map fst . sortOf snd . assocs

prob84 :: Int -> String
prob84 i = show_dist (iterate next_dist init_dist !! i)

main :: IO String
main = return $ prob84 100
-- 101524
