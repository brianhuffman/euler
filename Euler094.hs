module Euler094 where

------------------------------------------------------------------------------
-- 94. Investigating almost equilateral triangles with integral sides and area.
{-
It is easily proved that no equilateral triangle exists with integral length
sides and integral area. However, the almost equilateral triangle 5-5-6 has an
area of 12 square units.

We shall define an almost equilateral triangle to be a triangle for which two
sides are equal and the third differs by no more than one unit.

Find the sum of the perimeters of every almost equilateral triangle with
integral side lengths and area and whose perimeters do not exceed one billion
(1,000,000,000).
-}

{-
Almost-equilateral triangle 5-5-6
corresponds to pythagorean triple (3,4,5).

In general, every almost-equilateral triangle corresponds
to a principal pythagorean triple (a,b,c) where c = 2a-1 or c = 2a+1.

(It must be principal because gcd (a, c) = 1.)

Pythagorean triples (a,b,c) where c = 2a-1:
Perimeter = 2a + 2c = 6a - 2
a^2 + b^2 = c^2
a^2 + b^2 = (2a-1)^2
a^2 + b^2 = 4a^2 - 4a + 1
3a^2 - b^2 - 4a + 1 = 0

[(3,4,5),(33,56,65),(451,780,901),(6273,10864,12545),(87363,151316,174725),(1216801,2107560,2433601)]

a' = 7a + 4b - 4
b' = 12a + 7b - 8

Pythagorean triples (a,b,c) where c = 2a+1:
Perimeter = 2a + 2c = 6a + 2
a^2 + b^2 = c^2
a^2 + b^2 = (2a+1)^2
a^2 + b^2 = 4a^2 + 4a + 1
3a^2 - b^2 + 4a + 1 = 0

[(8,15,17),(120,209,241),(1680,2911,3361),(23408,40545,46817),(326040,564719,652081)]

This transformation preserves (3a^2 - b^2 + 4a + 1):
a' = 7a + 4b + 4
b' = 12a + 7b + 8

Inverse transformation:
a = 7a' - 4b' + 4
b = -12a' + 7b' - 8
-}

prob94 n = sum (takeWhile (<= n) xs) + sum (takeWhile (<= n) ys)
  where
    xs = f 3 4
    ys = g 8 15
    f a b = 6*a - 2 : f (7*a + 4*b - 4) (12*a + 7*b - 8)
    g a b = 6*a + 2 : g (7*a + 4*b + 4) (12*a + 7*b + 8)

main :: IO String
main = return $ show $ prob94 (10^9)
-- 518408346
