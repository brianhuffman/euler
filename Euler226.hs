module Euler226 where

{-

Problem 226
02 January 2009

The blancmange curve is the set of points (x,y) such that 0 ≤ x ≤ 1
and y = SUM n. s(2^n x)/2^n, where s is the sawtooth function: s(x) =
the distance from x to the nearest integer.

The area under the blancmange curve is equal to 1/2, shown in pink in
the diagram below.

[blancmange curve]

Let C be the circle with centre (1/4, 1/2) and radius 1/4, shown in
black in the diagram.

What area under the blancmange curve is enclosed by C?
Give your answer rounded to eight decimal places in the form 0.abcdefgh

-}

frac x = snd (properFraction x)

sawtooth x = if y > 0.5 then 1-y else y
  where y = frac x

sawtooth_area x
  | x >= 1.0 = 1/4 + sawtooth_area (x - 1)
  | x <= 0.5 = x^2 / 2
  | x >= 0.5 = 1/8 + (1-x)^2 / 2

blancmange x
  | y == 0.0 = 0
  | otherwise = sawtooth y + blancmange (2*y) / 2
  where
    y = frac x

-- integral from 0..x of blancmange(x)
blancmange_area x
  | x >= 1.0 = 1/2 + blancmange_area (x - 1)
  | x == 0.0 = 0
  | x <= 0.5 = (x^2/2) + blancmange_area (2*x) / 4
  | x >= 0.5 = 1/2 - blancmange_area (1 - x)

circle x = 1/2 - sqrt ((1/4)^2 - (1/4 - x)^2)

-- integral from 0..x of circle(x)
circle_area x = x/2 - theta/32 + (1/4 - x) * (1/2 -  y) / 2
  where
    theta = acos (1 - 4*x)
    y = circle x

{-

Question: Is the blancmange curve bounded above by a circle?

Is it true that, for all 0 <= x <= 1, that
 blancmange(x) <= sqrt(1 - (1-x)^2) ?

Probably.

-}


{-

Assume that the circle crosses the curve at x=0.5, and at exactly one
other point.

Search by bisection finds a crossing at
x = 7.890778796534192e-2

-}


inside (x, y) = (1/4 - x)^2 + (1/2 - y)^2 < (1/4)^2

bisect (x1, x2) = x : bisect next
  where
    x = (x1 + x2) / 2
    y = blancmange x
    next = if inside (x,y) then (x1,x) else (x,x2)

crossing = bisect (0.0, 0.5) !! 100

prob226 x = a1 - a2
  where
    a1 = blancmange_area 0.5 - blancmange_area x
    a2 = circle_area 0.5 - circle_area x

main :: IO String
main = return $ show_rounded 8 $ prob226 crossing
-- 0.11316017

show_rounded d x =
  show (floor x) ++ "." ++ replicate (d - length (show r)) '0' ++ show r
  where
    r = round (x * 10^d) `mod` (10^d)
