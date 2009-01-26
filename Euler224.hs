module Euler224 where

{-
Problem 224
26 December 2008

Let us call an integer sided triangle with sides a ≤ b ≤ c barely
obtuse if the sides satisfy

a^(2) + b^(2) = c^(2) - 1.

How many barely obtuse triangles are there with perimeter ≤ 75,000,000?
-}

{-
a^2 + b^2 = c^2 - 1

If (a,b,c) is a solution with a,b,c all positive, then we have the
inequalities a, b < c < a + b.

The only solution with a = 0 is (0,0,1).

If (a,b,c) is a solution, then (a',b',c') is also a solution:

a' = 2a +  b + 2c
b' =  a + 2b + 2c
c' = 2a + 2b + 3c

The transformation matrix has determinant 1, so it has an inverse with
integer coefficients:

|2 1 2|   | 2  1 -2|   |1 0 0|
|1 2 2| * | 1  2 -2| = |0 1 0|
|2 2 3|   |-2 -2  3|   |0 0 1|

(a - b) is an invariant of this transformation.

next (a,b,c) = (2a + b + 2c, a + 2b + 2c, 2a + 2b + 3c)
prev (a,b,c) = (2a + b - 2c, a + 2b - 2c, 3c - 2a - 2b)

c < a + b
2c < 2a + 2b
3c - 2a - 2b < c

a < c, b < c
a + b < 2c
2a + 2b < 4c
-c < 3c - 2a - 2b

We can conclude that for any positive solution (a,b,c),
|3c - 2a - 2b| < c.

This means that prev always gives back a "smaller" solution. We can
use this fact to generate all solutions from a few base cases.

The only positive integer solutions to |3c - 2a - 2b| = 0 are (1,2,2)
and (2,1,2).

0 < c'
0 < 3c - 2a - 2b
2a + 2b < 3c
(2a + 2b)^2 < (3c)^2
4a^2 + 4b^2 + 8ab < 9c^2
4a^2 + 4b^2 + 8ab < 9(c^2 - 1) + 9
4a^2 + 4b^2 + 8ab < 9(a^2 + b^2) + 9
4a^2 + 4b^2 + 8ab < 9a^2 + 9b^2 + 9
8ab < 5a^2 + 5b^2 + 9
0 < a^2 + b^2 + (4a^2 - 8ab + 4b^2) + 9
0 < a^2 + b^2 + 4(b - a)^2 + 9
This holds for all positive a and b.

0 < b'
0 < a + 2b - 2c
2c < a + 2b
(2c)^2 < (a + 2b)^2
4c^2 < a^2 + 4ab + 4b^2
4(c^2 + 1) - 4 < a^2 + 4ab + 4b^2
4(a^2 + b^2) - 4 < a^2 + 4ab + 4b^2
4a^2 + 4b^2 - 4 < a^2 + 4ab + 4b^2
3a^2 - 4 < 4ab
3a^2 < 4ab + 4
This always holds if a <= b.

a' <= b'
2a + b - 2c <= a + 2b - 2c
2a + b <= a + 2b
a <= b

Thus, for a positive solution (a,b,c) with 0 < a <= b, prev(a,b,c) =
(a',b',c') satisfies c' < c, a' <= b', and 0 < b'.

Furthermore, a' = 0 iff (a,b,c) = (2,2,3).

Therefore, there are 3 remaining cases for how each canonical triple
can be generated from a previous canonical triple.

1: (a,b,c) -> next(a,b,c)
2: (a,b,c) -> next(-a,b,c)
3: (a,b,c) -> next(-b,a,c)

-}

next :: (Int, Int, Int) -> (Int, Int, Int)
next (a, b, c) = (2*a + b + 2*c, a + 2*b + 2*c, 2*a + 2*b + 3*c)

prev :: (Int, Int, Int) -> (Int, Int, Int)
prev (a, b, c) = (2*a + b - 2*c, a + 2*b - 2*c, 3*c - 2*a - 2*b)

barely_obtuse_triangles :: Int -> [(Int, Int, Int)]
barely_obtuse_triangles m = f [(2,2,3)]
  where
    f [] = []
    f ((a,b,c):ts)
      | a + b + c > m = f ts
      | a == b = (a,b,c) : f (next(a,b,c) : next(-a,b,c) : ts)
      | otherwise = (a,b,c) : f (next(a,b,c) : next(-a,b,c) : next(-b,a,c) : ts)

-- prob224 m = length (barely_obtuse_triangles m)
prob224 :: Int -> Int
prob224 m = f [(2,2,3)] 0
  where
    f [] n = n
    f ((a,b,c):ts) n
      | a + b + c > m = f ts n
      | a == b = f (next(a,b,c) : next(-a,b,c) : ts) $! (n+1)
      | otherwise = f (next(a,b,c) : next(-a,b,c) : next(-b,a,c) : ts) $! (n+1)

main :: IO String
main = return $ show $ prob224 (75*10^6)

answer :: String
answer = "4137330"
