module Euler085 where
import SquareRoot
import EulerLib (triangle)

------------------------------------------------------------------------------
-- 85. Investigating the number of rectangles in a rectangular grid.
{-
By counting carefully it can be seen that a rectangular grid measuring 3 by 2
contains eighteen rectangles:

1x1 (6)
2x1 (4)
3x1 (2)
2x1 (3)
2x2 (2)
3x2 (1)

Although there exists no rectangular grid that contains exactly two million rectangles, find the area of the grid with the nearest solution.
-}

rectangles x y = triangle x * triangle y

{-
We want to find consecutive y and y' such that
tri x * tri y <= n <= tri x * tri y'
x*(x+1) * y*(y+1) <= 4*n <= x*(x+1) * y'*(y'+1)
y*(y+1) <= 4*n `div` x*(x+1) <= y'*(y'+1)
sqrt (y*(y+1)) <= sqrt (4*n `div` x*(x+1)) <= sqrt (y'*(y'+1))
y <= sqrt (4*n `div` x*(x+1)) <= y'
-}

close_rectangles n =
  [ (x, y, rectangles x y) |
    x <- takeWhile (\x -> triangle (x-1) ^ 2 < n) [1 ..],
    let y0 = square_root (2*n `div` triangle x),
    let r0 = rectangles x y0,
    y <- case compare r0 n of
           LT -> [y0, y0+1]
           EQ -> [y0]
           GT -> [y0, y0-1]
  ]

prob85 n = snd $ minimum
  [ (abs (n - c), x * y) | (x, y, c) <- close_rectangles n ]

main :: IO String
main = return $ show $ prob85 (2*10^6)
-- 2772

-- rectangles 36 77 = 1999998
-- 77*36 = 2772
