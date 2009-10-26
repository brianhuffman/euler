module Euler260 where
import Data.Array.Unboxed

{-

Problem 260
16 October 2009

A game is played with three piles of stones and two players.  At her
turn, a player removes one or more stones from the piles. However, if
she takes stones from more than one pile, she must remove the same
number of stones from each of the selected piles.

In other words, the player chooses some N>0 and removes:

    * N stones from any single pile; or
    * N stones from each of any two piles (2N total); or
    * N stones from each of the three piles (3N total).

The player taking the last stone(s) wins the game.

A winning configuration is one where the first player can force a win.
For example, (0,0,13), (0,11,11) and (5,5,5) are winning
configurations because the first player can immediately remove all
stones.

A losing configuration is one where the second player can force a win,
no matter what the first player does.

For example, (0,1,2) and (1,3,3) are losing configurations: any legal
move leaves a winning configuration for the second player.

Consider all losing configurations (x_(i),y_(i),z_(i)) where x_(i) ≤
y_(i) ≤ z_(i) ≤ 100.

We can verify that Σ(x_(i)+y_(i)+z_(i)) = 173895 for these.

Find Σ(x_(i)+y_(i)+z_(i)) where (x_(i),y_(i),z_(i)) ranges over the
losing configurations with x_(i) ≤ y_(i) ≤ z_(i) ≤ 1000.

-}

----------------------------------------------------------------------

type State = (Int, Int, Int)

type Bitmap = UArray (Int, Int) Bool

-- precondition on proj functions: x <= y <= z

proj1 :: State -> [(Int, Int)]
proj1 (x,y,z) = [(x,y), (x,z), (y,z)]

proj2 :: State -> [(Int, Int)]
proj2 (x,y,z) = [(x, z-y), (y, z-x), (z, y-x)]

proj3 :: State -> [(Int, Int)]
proj3 (x,y,z) = [(y-x, z-x)]

layer :: Int -> Int -> [State]
layer m s =
  [ (x,y,z) |
    let xmin = max 0 (s - 2*m),
    let xmax = s `div` 3,
    x <- [xmin .. xmax],
    let ymin = max x (s - x - m),
    let ymax = (s - x) `div` 2,
    y <- [ymin .. ymax],
    let z = s - x - y
  ]

losing :: Int -> [State]
losing m = go 0 b0 b0 b0
  where
    b0 :: Bitmap
    b0 = accumArray (||) False ((0,0),(m,m)) []
    go s b1 b2 b3
      | s > 3*m = []
      | otherwise = ps ++ go (s+1) b1' b2' b3'
      where
        ps = [ p | p <- layer m s,
               not (any (b1 !) (proj1 p)),
               not (any (b2 !) (proj2 p)),
               not (any (b3 !) (proj3 p))
             ]
        b1' = b1 // [ (i, True) | p <- ps, i <- proj1 p ]
        b2' = b2 // [ (i, True) | p <- ps, i <- proj2 p ]
        b3' = b3 // [ (i, True) | p <- ps, i <- proj3 p ]

prob260 :: Int -> Int
prob260 m = sum [ x + y + z | (x, y, z) <- losing m ]

{-
prob260 50 = 21750
prob260 100 = 173895
prob260 150 = 580752
prob260 200 = 1358608
prob260 250 = 2631659
prob260 300 = 4520336
prob260 400 = 10714131
prob260 500 = 20999138
prob260 600 = 
prob260 700 =
prob260 800 =
prob260 900 =
prob260 1000 = 167542057

-}

main :: IO String
main = return $ show $ prob260 1000

answer :: String
answer = "167542057"
