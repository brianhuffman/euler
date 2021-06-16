module Euler121 where
import EulerLib
import Data.Ratio
import Data.Array

------------------------------------------------------------------------------
-- 121. Investigate the game of chance involving coloured discs.
{-
A bag contains one red disc and one blue disc. In a game of chance a player
takes a disc at random and its colour is noted. After each turn the disc is
returned to the bag, an extra red disc is added, and another disc is taken at
random.

The player pays £1 to play and wins if they have taken more blue discs than red
discs at the end of the game.

If the game is played for four turns, the probability of a player winning is
exactly 11/120, and so the maximum prize fund the banker should allocate for
winning in this game would be £10 before they would expect to incur a loss.
Note that any payout will be a whole number of pounds and also includes the
original £1 paid to play the game, so in the example given the player actually
wins £9.

Find the maximum prize fund that should be allocated to a single game in which
fifteen turns are played.
-}

-- blue_prob m (b,n) =
-- probability of taking at least b blue, in n turns.
blue_prob m = a
  where
    a = funArray ((0,1),m) f
    f (0,n) = 1
    f (1,1) = 1%2
    f (b,1) = 0
    f (b,n) = (1%(n+1)) * a!(b-1,n-1) -- last draw is blue
            + (n%(n+1)) * a!(b,n-1)   -- last draw is red

probability (b,n) = blue_prob (b,n) ! (b,n)

payout n = floor (recip p)
  where
    b = (n `div` 2) + 1
    p = probability (b,n)

main :: IO String
main = return $ show $ payout 15
-- 2269
