module Euler116 where
import Data.List (zipWith3)

------------------------------------------------------------------------------
-- 116. Investigating the number of ways of replacing square tiles with one of three coloured tiles.
{-
A row of five black square tiles is to have a number of its tiles replaced
with coloured oblong tiles chosen from red (length two), green (length three),
or blue (length four).

If red tiles are chosen there are exactly seven ways this can be done.

[][]X  []X[]  X[][]

If green tiles are chosen there are three ways.
		
[ ]XX  X[ ]X  XX[ ]
And if blue tiles are chosen there are two ways.

X[  ]  [  ]X

Assuming that colours cannot be mixed there are 7 + 3 + 2 = 12 ways of
replacing the black tiles in a row measuring five units in length.

How many different ways can the black tiles in a row measuring fifty units
in length be replaced if colours cannot be mixed and at least one coloured
tile must be used?

NOTE: This is related to problem 117.
-}

-- replacing square tiles with 0 or more length-l tiles
fill_count :: Int -> [Integer]
fill_count l = xs
  where
    xs = replicate l 1 ++ ys
    ys = zipWith (+) xs (1 : ys)

-- replacing square tiles with 1 or more length-2, 3 or 4 tiles
prob116 :: [Integer]
prob116 = zipWith3 f (fill_count 2) (fill_count 3) (fill_count 4)
  where f r g b = r + g + b - 3
-- prob116 !! 5 = 12
-- prob116 !! 50 = 20492570929

main :: IO String
main = return $ show $ prob116 !! 50
-- 20492570929


