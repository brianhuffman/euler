module Euler260 where
import Data.Word
import Data.Bits
--import Data.Array
--import EulerLib (funArray)
import UnboxedMemo

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


-- Each bit specifies whether a move in that direction
-- can result in a losing state for the other player.

--winning :: Int -> Array (Int, Int, Int) Word8
winning :: Int -> UMemo (Int, Int, Int) Word8
winning m = a
  where
--    a = funArray ((0,0,0), (m,m,m)) f
--    f' = (a !)
    a = newUMemo ((0,0,0), (m,m,m)) f
    f' = readUMemo a
    getbit b 0 = bit b
    getbit b x = bit b .&. x
    f (i,j,k) | j < i = flip12 (f' (j,i,k))
    f (i,j,k) | k < j = flip23 (f' (i,k,j))
    f (i,j,k) = foldl (.|.) 0 ds
      where
        d111 | 0 `elem` [i,j,k] = 0
             | otherwise        = getbit 7 (f' (i-1, j-1, k-1))
        d110 | 0 `elem` [i,j  ] = 0
             | otherwise        = getbit 6 (f' (i-1, j-1, k  ))
        d101 | 0 `elem` [i,  k] = 0
             | otherwise        = getbit 5 (f' (i-1, j  , k-1))
        d100 | 0 `elem` [i    ] = 0
             | otherwise        = getbit 4 (f' (i-1, j  , k  ))
        d011 | 0 `elem` [  j,k] = 0
             | otherwise        = getbit 3 (f' (i  , j-1, k-1))
        d010 | 0 `elem` [  j  ] = 0
             | otherwise        = getbit 2 (f' (i  , j-1, k  ))
        d001 | 0 `elem` [    k] = 0
             | otherwise        = getbit 1 (f' (i  , j  , k-1))
        ds = [d111, d110, d101, d100, d011, d010, d001]

flip12 :: Word8 -> Word8
flip12 w = foldl (.|.) 0 [d111, d110, d101, d100, d011, d010, d001]
  where
    d111 = if testBit w 7 then bit 7 else 0
    d110 = if testBit w 6 then bit 6 else 0
    d101 = if testBit w 3 then bit 5 else 0
    d100 = if testBit w 2 then bit 4 else 0
    d011 = if testBit w 5 then bit 3 else 0
    d010 = if testBit w 4 then bit 2 else 0
    d001 = if testBit w 1 then bit 1 else 0

flip23 :: Word8 -> Word8
flip23 w = foldl (.|.) 0 [d111, d110, d101, d100, d011, d010, d001]
  where
    d111 = if testBit w 7 then bit 7 else 0
    d110 = if testBit w 5 then bit 6 else 0
    d101 = if testBit w 6 then bit 5 else 0
    d100 = if testBit w 4 then bit 4 else 0
    d011 = if testBit w 3 then bit 3 else 0
    d010 = if testBit w 1 then bit 2 else 0
    d001 = if testBit w 2 then bit 1 else 0

losings :: Int -> [(Int, Int, Int)]
losings m =
  [ (i, j, k) |
    i <- [0 .. m],
    j <- [i .. m],
    k <- [j .. m],
    readUMemo a (i, j, k) == 0
--    a ! (i, j, k) == 0
  ]
  where a = winning m

prob260 :: Int -> Int
prob260 m = sum [ i+j+k | (i,j,k) <- losings m ]

{-
prob260 50 = 21750
prob260 100 = 173895
prob260 150 = 580752
prob260 200 = 1358608
prob260 250 = 2631659
prob260 300 = 4520336
prob260 400 = 10714131
prob260 500 = 20999138
prob260 1000 = 167542057

-}

main :: IO String
main = return $ show $ prob260 1000

answer :: String
answer = "167542057"
