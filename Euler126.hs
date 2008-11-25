module Euler126 where
import EulerLib
import Data.Array.Unboxed

------------------------------------------------------------------------------
-- 126. Exploring the number of cubes required to cover every visible face on a cuboid.
{-
The minimum number of cubes to cover every visible face on a cuboid measuring
3 x 2 x 1 is twenty-two.

If we then add a second layer to this solid it would require forty-six cubes
to cover every visible face, the third layer would require seventy-eight cubes,
and the fourth layer would require one-hundred and eighteen cubes to cover
every visible face.

However, the first layer on a cuboid measuring 5 x 1 x 1 also requires
twenty-two cubes; similarly the first layer on cuboids measuring 5 x 3 x 1,
7 x 2 x 1, and 11 x 1 x 1 all contain forty-six cubes.

We shall define C(n) to represent the number of solids that contain n cubes
in one of its layers. So C(22) = 2, C(46) = 4, C(78) = 5, and C(118) = 8.

It turns out that 154 is the least value of n for which C(n) = 10.

Find the least value of n for which C(n) = 1000.
-}

-- how many cubes in layer n, covering a*b*c solid
layer_size :: Int -> Int -> Int -> Int -> Int
layer_size a b c n =
  2*(a*b + a*c + b*c) +
  4*(a + b + c)*(n-1) +
  8*(triangle (n-2))

num_solids :: Int -> UArray Int Int
num_solids m = accumArray (+) 0 (0,m-1) (zip xs (repeat 1))
  where
   xs =
    [ x |
      a <- takeWhile (\a -> layer_size a a a 1 < m) [1 ..],
      b <- takeWhile (\b -> layer_size a b b 1 < m) [a ..],
      c <- takeWhile (\c -> layer_size a b c 1 < m) [b ..],
      x <- takeWhile (< m) $ map (layer_size a b c) [1 ..] ]

-- least n such that num_solids m ! n == x
prob126 :: Int -> Int -> Int
prob126 m x = fst $ head $ filter ((==x) . snd) $ assocs $ num_solids m

-- 20000 is upper bound on search (found by trial and error)
main :: IO String
main = return $ show $ prob126 20000 1000
-- 18522
