module Euler086 where
import EulerLib
import Pythagorean

------------------------------------------------------------------------------
-- 86. Exploring the shortest path from one corner of a cuboid to another.
{-
A spider, S, sits in one corner of a cuboid room, measuring 6 by 5 by 3, and a
fly, F, sits in the opposite corner. By travelling on the surfaces of the room
the shortest "straight line" distance from S to F is 10 and the path is shown
on the diagram.

However, there are up to three "shortest" path candidates for any given cuboid
and the shortest route is not always integer.

By considering all cuboid rooms up to a maximum size of M by M by M, there are
exactly 2060 cuboids for which the shortest distance is integer when M=100, and
this is the least value of M for which the number of solutions first exceeds
two thousand; the number of solutions is 1975 when M=99.

Find the least value of M such that the number of solutions first exceeds one
million.
-}

{-
For dimensions x,y,z with x <= y <= z
(x+y)^2 + z^2  must be a perfect square
-}

type Cuboid = (Int, Int, Int)

-- cuboids m = list of (a,b,c) with m >= a >= b>= c,
-- such that a^2 + (b+c)^2 is a perfect square.
cuboids :: Int -> [Cuboid]
cuboids m = concatMap f ts'
  where
    ts = pythagorean_triples (6*m)
    ts' = filter (\(a,b,c) -> a <= m && b <= 2*m) ts
    f (a,b,_) = g a b ++ (if b <= m then g b a else [])
    -- how many ways to split b = c+d, where 1 <= d <= c <= a ?
    g a b = [ (a,c,d) | c <- [1 .. a], let d = b - c, 1 <= d, d <= c ]

-- num_cuboids m = length (cuboids m)
num_cuboids :: Int -> Int
num_cuboids m = sum $ map f ts'
  where
    ts = pythagorean_triples (6*m)
    ts' = filter (\(a,b,c) -> a <= m && b <= 2*m) ts
    f (a,b,_) = g a b + (if b <= m then g b a else 0)
    -- how many ways to split b = c+d, where 1 <= d <= c <= a ?
    g a b = max 0 (min a (b-1) - ((b+1)`div`2) + 1)

-- prob86 l = least m such that num_cuboids m > l.
prob86 :: Int -> Int
prob86 l = either (+1) (+1) $
  binary_search
    (\m -> if num_cuboids m <= l then GT else LT)

main :: IO String
main = return $ show $ prob86 (10^6)
-- 1818

