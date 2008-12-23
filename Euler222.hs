module Euler222 where
import Data.List (sort)

{-
Problem 222
19 December 2008

What is the length of the shortest pipe, of internal radius 50mm, that
can fully contain 21 balls of radii 30mm, 31mm, ..., 50mm?

Give your answer in micrometres (10^(-6) m) rounded to the nearest
integer.
-}

{-
Note that all ball diameters are greater than half the tube diameter.
Thus all ball centers will lie on the same plane.

Let d = diameter of outer tube.
Let c = sum of radii of two adjacent balls.

Vertical distance between centers:
t = sqrt (2cd - d^2)
t = d * sqrt (2c/d - 1)

Savings compared to vertical stacking:
s = c - d * sqrt (2c/d - 1)

This function is maximized at c = d/2, zero at c = d.  The derivative
is steepest at c = d/2. Thus to maximize total savings, we should pair
the smallest balls together, and put the largest balls at the ends.

The problem is essentially a search problem to find the optimal
ordering of the balls. We can constrain the search by examining some
local optimizations; for an optimal configuration, no local
optimizations will be possible.

Local optimization: reversing sequence of balls at end.
  Configuration: [..a,y..x] with x < y.
  The pairing a-x saves more than the pairing a-y.
  Thus [..a,x..y] is better than [..a,y..x].

This means that a ball at the end must be larger than every ball in
the middle. Therefore, the two balls at the ends must be the two
largest balls.

Local optimization: reversing sequence of balls in middle.
  Four balls: [..a,y..x,b..] with a < b iff x < y.
  The pairings a-x and y-b save more than the pairings a-y and x-b.
  Thus [..a,x..y,b..] is better than [..a,y..x,b..].

Consider a configuration [..a..b..], with a > b, where all values
positioned between a and b are smaller than b. Let c be the
next-largest value positioned between a and b. We will show that if c
is not already adjacent to a, then it is always beneficial to reverse
a subsequence to move c adjacent to a.

Case 1: c is adjacent to b. The configuration is [..a,x..c,b..].
We have a > b, but x < c, so it is beneficial to reverse [x..c].

Case 2: c is apart from b. The configuration is [..a,x..c,y..b..].
We have a > y, but x < c, so it is beneficial to reverse [x..c].

Repeatedly applying this optimization to any initial configuration, we
end up with an arrangement where the largest value is at one end, the
second largest is at the other end, subsequent values are placed at
alternating ends, with the smallest value in the middle. This
alternating configuration will always be optimal.

-}

type R = Double

funny_sort :: (Ord a) => [a] -> [a]
funny_sort xs = f [] (reverse (sort xs))
  where
    f ts (x : y : zs) = x : f (y : ts) zs
    f ts [x] = x : ts
    f ts [] = ts

pipe_length :: R -> [R] -> R
pipe_length d rs = 2 * sum rs - f rs
  where
    f (r1 : r2 : rs) = savings (r1 + r2) d + f (r2 : rs)
    f _ = 0

savings :: R -> R -> R
savings c d = c - d * sqrt (2 * c/d - 1)

minimum_length :: R -> [R] -> R
minimum_length r rs = pipe_length (2*r) (funny_sort rs)

main :: IO String
main = return $ show $ round $ minimum_length 50 [30 .. 50] * 1000
