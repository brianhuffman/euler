module Euler215 where
import EulerLib
import Array
import Bits

{-
Consider the problem of building a wall out of 2x1 and 3x1 bricks
(horizontal x vertical dimensions) such that, for extra strength,
the gaps between horizontally-adjacent bricks never line up in
consecutive layers, i.e. never form a "running crack".

For example, the following 9x3 wall is not acceptable due to the
running crack shown in red:

  |--|-|-|-|
  |-|-|--|-|
  |--|--|--|

There are eight ways of forming a crack-free 9x3 wall, written
W(9,3) = 8.

Calculate W(32,10).
-}

show_binary 0 = ""
show_binary n = (if even n then '0' else '1') : show_binary (n `div` 2)

one_layer :: Int -> [Integer]
one_layer n
  | n > 3 = [ 8*k+4 | k <- one_layer (n-3) ] ++
            [ 4*k+2 | k <- one_layer (n-2) ]
  | n == 3 = [0]
  | n == 2 = [0]
  | n == 0 = [0]
  | otherwise = []

prob215 w h = sum [ b!(h,i) | i <- [1 .. k] ]
  where
    ps = one_layer w
    k = length ps
    a = listArray (1, k) (map compats ps)
    compats p = [ i | (i, p') <- zip [1..] ps, p .&. p' == 0 ]
    b = funArray ((1,1),(h,k)) f
    f (1, i) = 1
    f (n, i) = sum [ b!(n-1, j) | j <- a!i ]

main :: IO String
main = return $ show $ prob215 32 10
-- 806844323190414

{-
Analysis:

No brick can span two cracks, like this:
 |-|
|---|
because the bricks are constrained by length.

Therefore all bricks must span exactly one crack.

For bricks of length 2, the crack can only be in
one place:
  |
 |-|

For bricks of length 3, the crack can be in one of
two places:
  |      |
 |--|  |--|

However, when two 3-bricks are side by side, the
crack positions are constrained:
  |-|     |--|      |--|    |---|  (last one not possible)
|--|--|  |--|--|  |--|--|  |--|--|
Only the first 3 are allowed.

Construction by cases:
Consider a sequence of n 3-bricks, surrounded by a pair of 2-bricks.

n = 0: 1 arrangement.
 |-|
|-|-|

n = 1: 2 arrangements.
 |-|--|    |--|-|
|-|--|-|  |-|--|-|

n = 2: 3 arrangements.
 |-|--|--|    |--|-|--|    |--|--|-|
|-|--|--|-|  |-|--|--|-|  |-|--|--|-| 

For arbitrary n: n+1 arrangements.
|-|--|--|--|--|-|

----------------------------------
-- W(32,10)

No free bricks: 1 arrangement.
|--|-|-|-|-|-|-|-|-|-|-|-|-|-|--|
|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|
|--|-|-|-|-|-|-|-|-|-|-|-|-|-|--|
|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|
|--|-|-|-|-|-|-|-|-|-|-|-|-|-|--|
|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|
|--|-|-|-|-|-|-|-|-|-|-|-|-|-|--|
|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|
|--|-|-|-|-|-|-|-|-|-|-|-|-|-|--|
|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|

One free brick. The free brick can move
left or right by one unit per layer.
|-|-|-|-|-|  |-|-|-|-|-|-|-|-|--|
|--|-|-|-|  |-|-|-|-|-|-|-|-|-|-|
|-|-|-|-|  |-|-|-|-|-|-|-|-|-|--|
|--|-|-|-|  |-|-|-|-|-|-|-|-|-|-|
|-|-|-|-|-|  |-|-|-|-|-|-|-|-|--|
|--|-|-|-|-|  |-|-|-|-|-|-|-|-|-|
|-|-|-|-|-|-|  |-|-|-|-|-|-|-|--|
|--|-|-|-|-|-|  |-|-|-|-|-|-|-|-|
|-|-|-|-|-|-|-|  |-|-|-|-|-|-|--|
|--|-|-|-|-|-|  |-|-|-|-|-|-|-|-|

Nine free bricks: 1 arrangement.
|-|  |  |  |  |  |  |  |  |  |--|
|--|  |  |  |  |  |  |  |  |  |-|
|-|  |  |  |  |  |  |  |  |  |--|
|--|  |  |  |  |  |  |  |  |  |-|
|-|  |  |  |  |  |  |  |  |  |--|
|--|  |  |  |  |  |  |  |  |  |-|
|-|  |  |  |  |  |  |  |  |  |--|
|--|  |  |  |  |  |  |  |  |  |-|
|-|  |  |  |  |  |  |  |  |  |--|
|--|  |  |  |  |  |  |  |  |  |-|

|-|  |  |  |  |  |  |  |  |-|-|-|
|--|  |  |  |  |  |  |  |  |-|--|
|-|  |-|  |  |  |  |  |  |  |-|-|
|--|  |  |  |  |  |-|  |  |  |--|
|-|  |  |-|  |  |  |  |-|  |  |-|
...
-}
