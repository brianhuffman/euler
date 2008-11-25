module Euler117 where

------------------------------------------------------------------------------
-- 117. Investigating the number of ways of tiling a row using different-sized tiles.
{-
Using a combination of black square tiles and oblong tiles chosen from: red
tiles measuring two units, green tiles measuring three units, and blue tiles
measuring four units, it is possible to tile a row measuring five units in
length in exactly fifteen different ways.
				
xxxxx RRxxx xRRxx xxRRx
xxxRR RRRRx RRxRR xRRRR
GGGxx xGGGx xxGGG RRGGG
GGGRR BBBBx xBBBB

How many ways can a row measuring fifty units in length be tiled?

NOTE: This is related to problem 116.
-}

--f(n) = f(n-1) + f(n-2) + f(n-3) + f(n-4)

prob117 :: [Integer]
prob117 = f 0 0 0 1
  where f a b c d = d : f b c d (a + b + c + d)
-- prob117 !! 5 = 15
-- prob117 !! 50 = 100808458960497

main :: IO String
main = return $ show $ prob117 !! 50
