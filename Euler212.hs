module Euler212 where
import Lagged (lagged_fibonacci)

{-
Problem 212
11 October 2008

An axis-aligned cuboid, specified by parameters { (x_(0),y_(0),z_(0)),
(dx,dy,dz) }, consists of all points (X,Y,Z) such that x_(0) ≤ X ≤
x_(0)+dx, y_(0) ≤ Y ≤ y_(0)+dy and z_(0) ≤ Z ≤ z_(0)+dz. The volume of
the cuboid is the product, dx × dy × dz. The combined volume of a
collection of cuboids is the volume of their union and will be less
than the sum of the individual volumes if any cuboids overlap.

Let C_(1),...,C_(50000) be a collection of 50000 axis-aligned cuboids
such that C_(n) has parameters

      x_(0) = S_(6n-5) modulo 10000
      y_(0) = S_(6n-4) modulo 10000
      z_(0) = S_(6n-3) modulo 10000
      dx = 1 + (S_(6n-2) modulo 399)
      dy = 1 + (S_(6n-1) modulo 399)
      dz = 1 + (S_(6n) modulo 399)

where S_(1),...,S_(300000) come from the "Lagged Fibonacci Generator":

      For 1 ≤ k ≤ 55, S_(k) = [100003 - 200003k + 300007k^(3)]
         (modulo 1000000)

      For 56 ≤ k, S_(k) = [S_(k-24) + S_(k-55)]
         (modulo 1000000)

Thus, C_(1) has parameters {(7,53,183),(94,369,56)}, C_(2) has
parameters {(2383,3563,5079),(42,212,344)}, and so on.

The combined volume of the first 100 cuboids, C_(1),...,C_(100), is
723581599.

What is the combined volume of all 50000 cuboids, C_(1),...,C_(50000) ? 

-}

type Cuboid = (Int, Int, Int, Int, Int, Int)

cuboids :: Int -> [Cuboid]
cuboids n = take n (cs lagged_fibonacci)
  where
    cs (a:b:c:d:e:f:xs) = (x0,y0,z0,x1,y1,z1) : cs xs
      where
        x0 = a `mod` 10000
        y0 = b `mod` 10000
        z0 = c `mod` 10000
        dx = 1 + d `mod` 399
        dy = 1 + e `mod` 399
        dz = 1 + f `mod` 399
        x1 = x0 + dx
        y1 = y0 + dy
        z1 = z0 + dz

split_x :: Int -> [Cuboid] -> ([Cuboid], [Cuboid])
split_x sx cs = f cs [] []
  where
    f [] ls rs = (ls, rs)
    f (c@(x0,y0,z0,x1,y1,z1):cs) ls rs
      | x1 <= sx = f cs (c:ls) rs
      | sx <= x0 = f cs ls (c:rs)
      | otherwise = f cs (cl:ls) (cr:rs)
          where
             cl = (x0,y0,z0,sx,y1,z1)
             cr = (sx,y0,z0,x1,y1,z1)

split_y :: Int -> [Cuboid] -> ([Cuboid], [Cuboid])
split_y sy cs = f cs [] []
  where
    f [] ls rs = (ls, rs)
    f (c@(x0,y0,z0,x1,y1,z1):cs) ls rs
      | y1 <= sy = f cs (c:ls) rs
      | sy <= y0 = f cs ls (c:rs)
      | otherwise = f cs (cl:ls) (cr:rs)
          where
             cl = (x0,y0,z0,x1,sy,z1)
             cr = (x0,sy,z0,x1,y1,z1)

split_z :: Int -> [Cuboid] -> ([Cuboid], [Cuboid])
split_z sz cs = f cs [] []
  where
    f [] ls rs = (ls, rs)
    f (c@(x0,y0,z0,x1,y1,z1):cs) ls rs
      | z1 <= sz = f cs (c:ls) rs
      | sz <= z0 = f cs ls (c:rs)
      | otherwise = f cs (cl:ls) (cr:rs)
          where
             cl = (x0,y0,z0,x1,y1,sz)
             cr = (x0,y0,sz,x1,y1,z1)

volume :: Cuboid -> Integer
volume (x0,y0,z0,x1,y1,z1) = dx * dy * dz
  where
    dx = toInteger (x1 - x0)
    dy = toInteger (y1 - y0)
    dz = toInteger (z1 - z0)

total_volume1 :: [Cuboid] -> Integer
total_volume1 [] = 0
total_volume1 (c@(x0,y0,z0,x1,y1,z1):cs0) =
    volume c + sum [v1,v2,v3,v4,v5,v6]
  where
    (ds1, cs1) = split_x x0 cs0
    (ds2, cs2) = split_y y0 cs1
    (ds3, cs3) = split_z z0 cs2
    (cs4, ds4) = split_x x1 cs3
    (cs5, ds5) = split_y y1 cs4
    (cs6, ds6) = split_z z1 cs5
    v1 = total_volume2 ds1
    v2 = total_volume3 ds2
    v3 = total_volume1 ds3
    v4 = total_volume2 ds4
    v5 = total_volume3 ds5
    v6 = total_volume1 ds6

total_volume2 :: [Cuboid] -> Integer
total_volume2 [] = 0
total_volume2 (c@(x0,y0,z0,x1,y1,z1):cs0) =
    volume c + sum [v1,v2,v3,v4,v5,v6]
  where
    (ds1, cs1) = split_y y0 cs0
    (ds2, cs2) = split_z z0 cs1
    (ds3, cs3) = split_x x0 cs2
    (cs4, ds4) = split_y y1 cs3
    (cs5, ds5) = split_z z1 cs4
    (cs6, ds6) = split_x x1 cs5
    v1 = total_volume3 ds1
    v2 = total_volume1 ds2
    v3 = total_volume2 ds3
    v4 = total_volume3 ds4
    v5 = total_volume1 ds5
    v6 = total_volume2 ds6

total_volume3 :: [Cuboid] -> Integer
total_volume3 [] = 0
total_volume3 (c@(x0,y0,z0,x1,y1,z1):cs0) =
    volume c + sum [v1,v2,v3,v4,v5,v6]
  where
    (ds1, cs1) = split_z z0 cs0
    (ds2, cs2) = split_x x0 cs1
    (ds3, cs3) = split_y y0 cs2
    (cs4, ds4) = split_z z1 cs3
    (cs5, ds5) = split_x x1 cs4
    (cs6, ds6) = split_y y1 cs5
    v1 = total_volume1 ds1
    v2 = total_volume2 ds2
    v3 = total_volume3 ds3
    v4 = total_volume1 ds4
    v5 = total_volume2 ds5
    v6 = total_volume3 ds6

main :: IO String
main = return $ show $ total_volume1 (cuboids 50000)

answer :: String
answer = "328968937309"
