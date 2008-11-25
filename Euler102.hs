module Euler102 where

------------------------------------------------------------------------------
-- 102. For how many triangles in the text file does the interior contain the origin?
{-
Three distinct points are plotted at random on a Cartesian plane, for
which -1000 <= x, y <= 1000, such that a triangle is formed.

Consider the following two triangles:

A(-340,495), B(-153,-910), C(835,-947)

X(-175,41), Y(-421,-714), Z(574,-645)

It can be verified that triangle ABC contains the origin, whereas triangle
XYZ does not.

Using triangles.txt, a 27K text file containing the co-ordinates of one
thousand "random" triangles, find the number of triangles for which the
interior contains the origin.

NOTE: The first two examples in the file represent the triangles in the
example given above.
-}

type Point = (Int, Int)

compare_origin :: Point -> Point -> Ordering
compare_origin (x1,y1) (x2,y2) = compare (x1*y2) (x2*y1)
-- LT means clockwise
-- EQ means straight
-- GT means counter-clockwise

type Triangle = ((Int, Int), (Int, Int), (Int, Int))

triangle_contains_origin :: Triangle -> Bool
triangle_contains_origin ((x1,y1),(x2,y2),(x3,y3)) =
  case (a, b, c) of
    (LT, LT, LT) -> True
    (LT, LT, GT) -> False
    (LT, GT, _) -> False
    (GT, LT, _) -> False
    (GT, GT, LT) -> False
    (GT, GT, GT) -> True
    _ -> error (show (a,b,c))
  where
    a = compare_origin (x1,y1) (x2,y2)
    b = compare_origin (x2,y2) (x3,y3)
    c = compare_origin (x3,y3) (x1,y1)

l2t :: [Int] -> Triangle
l2t [a,b,c,d,e,f] = ((a,b),(c,d),(e,f))

prob102 :: [Triangle] -> Int
prob102 ts = length $ filter triangle_contains_origin ts

trianglesTxt :: IO [Triangle]
trianglesTxt = readFile "triangles.txt" >>= return .
  map (\l -> l2t (read ("[" ++ l ++ "]"))) . lines

main :: IO String
main = trianglesTxt >>= return . show . prob102
-- 228

