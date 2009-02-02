module Euler161 where
import Data.Array

{-
Problem 161
21 September 2007

A triomino is a shape consisting of three squares joined via the
edges.  There are two basic forms:

 +--+--+  +--+--+--+
 |  |  |  |  |  |  |
 +--+--+  +--+--+--+
 |  |
 +--+

If all possible orientations are taken into account there are six:

    A        B        C        D      E        F
 +--+--+  +--+--+  +--+        +--+  +--+  +--+--+--+
 |  |  |  |  |  |  |  |        |  |  |  |  |  |  |  |
 +--+--+  +--+--+  +--+--+  +--+--+  +--+  +--+--+--+
 |  |        |  |  |  |  |  |  |  |  |  |
 +--+        +--+  +--+--+  +--+--+  +--+
                                     |  |
                                     +--+

Any n by m grid for which nxm is divisible by 3 can be tiled with
triominoes.  If we consider tilings that can be obtained by reflection
or rotation from another tiling as different there are 41 ways a 2 by
9 grid can be tiled with triominoes:

In how many ways can a 9 by 12 grid be tiled in this way by
triominoes?
-}

{-
Analysis:

Original problem: Find tilings of 9x12 grid:
 XXXXXXXXX
 XXXXXXXXX
 XXXXXXXXX
 XXXXXXXXX
 XXXXXXXXX
 XXXXXXXXX
 XXXXXXXXX
 XXXXXXXXX
 XXXXXXXXX
 XXXXXXXXX
 XXXXXXXXX
 XXXXXXXXX

As a generalization, we will count tilings of incomplete grids:
      XXX
   XXXXXXX
 XXXXXXXXX


-}


type I = Int
type Tuple = Int

l2t :: [I] -> Tuple
l2t xs = foldr (\x t -> x + 4*t) 0 xs `div` 3

t2l :: Tuple -> [I]
t2l n = f 9 (n*3)
  where f 0 _ = []
        f l n = let (q,r) = divMod n 4 in r : f (l-1) q

minb :: Tuple
minb = l2t [0,0,0,0,0,0,0,0,0]

maxb :: Tuple
maxb = l2t [3,3,3,3,3,3,3,3,3]

bounds9 :: (Tuple, Tuple)
bounds9 = (minb, maxb)

mkArray :: (Tuple -> a) -> Array Tuple a
mkArray f = listArray bounds9 (map f (range bounds9))

next_tuples :: Array Tuple [Tuple]
next_tuples = mkArray (map l2t . f . t2l)
  where
    f (3:3:3:3:xs) =
      [1:1:2:2:xs, 1:1:1:3:xs, 2:2:2:3:xs,
       2:1:3:3:xs, 1:2:3:3:xs, 0:3:3:3:xs]
    f (3:3:3:xs) = [1:1:1:xs, 2:2:2:xs, 2:1:3:xs, 1:2:3:xs, 0:3:3:xs]
    f (3:3:xs) = [2:1:xs, 1:2:xs, 0:3:xs]
    f (3:2:xs) = [1:1:xs, 0:2:xs]
    f (3:xs) = [0:xs]
    f (2:3:xs) = (1:1:xs) : map (2:) (f (3:xs))
    f (x:xs) = map (x:) (f xs)
    f [] = []
{-
XXXX    ....  ...X  ...X  ..XX  ..XX  .XXX
XXXX -> ..XX  ...X  XXXX  X.XX  .XXX  .XXX
XXXX    XXXX  XXXX  XXXX  XXXX  XXXX  .XXX

X.    ..  ..
XX -> ..  .X
XX    XX  .X

.X    ..  ..
XX -> ..  .X
XX    XX  .X
-}

arr0 :: Array Tuple Integer
arr0 = accumArray (+) 0 bounds9 [(maxb, 1)]

incr :: Tuple -> Tuple
incr = l2t . map (+1) . t2l

next_arr :: Array Tuple Integer -> Array Tuple Integer
next_arr a = b
  where
    b = mkArray f
    f x =
      let xs = next_tuples ! x
      in if null xs then a ! incr x
                    else sum [ b ! x' | x' <- xs ]

prob161 :: Int -> Integer
prob161 n = (iterate next_arr arr0 !! n) ! maxb

main :: IO String
main = return $ show $ prob161 12
-- 20574308184277971
