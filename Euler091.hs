module Euler091 where

------------------------------------------------------------------------------
-- 91. Find the number of right angle triangles in the quadrant.
{-
The points P (x1, y1) and Q (x2, y2) are plotted at integer co-ordinates and
are joined to the origin, O (0,0), to form ΔOPQ.

There are exactly fourteen triangles containing a right angle that can be
formed when each co-ordinate lies between 0 and 2 inclusive; that is,
0 <= x1, y1, x2, y2 <= 2.

Given that 0 <= x1, y1, x2, y2 <= 50, how many right triangles can be formed?
-}

type Point = (Int, Int)

is_right_angle :: Point -> Point -> Bool
is_right_angle (x1,y1) (x2,y2) = x1 * x2 + y1 * y2 == 0

prob91 :: Int -> [(Point, Point)]
prob91 m = ts1 ++ ts2
  where
    ts1 = [ ((x, 0), (0, y)) | x <- [1 .. m], y <- [1 .. m] ]
    ts2 = [ (p, q) |
            p@(x1, y1) <- points,
            q@(x2, y2) <- points,
            p /= q,
            is_right_angle (x1, y1) (x1-x2, y1-y2) ]
    points = tail [(x,y) | x <- [0 .. m], y <- [0 .. m]]

main :: IO String
main = return $ show $ length $ prob91 50
-- 14234

-- TODO: make this faster