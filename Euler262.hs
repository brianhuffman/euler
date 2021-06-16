module Euler262 where

{---------------------------------------------------------------------
Mountain Range

Problem 262
30 October 2009

The following equation represents the continuous topography of a
mountainous region, giving the elevation h at any point (x,y):

h(x,y) = A(x,y) * e^-|B(x,y)|

A(x,y) = 5000 - (x^2 + y^2 + xy)/200 + 25(x+y)/2

B(x,y) = (x^2 + y^2)/1000000 - 3(x+y)/2000 + 7/10

A mosquito intends to fly from A(200,200) to B(1400,1400), without
leaving the area given by 0 ≤ x, y ≤ 1600.

Because of the intervening mountains, it first rises straight up to a
point A', having elevation f. Then, while remaining at the same
elevation f, it flies around any obstacles until it arrives at a point
B' directly above B.

First, determine f_(min) which is the minimum constant elevation
allowing such a trip from A to B, while remaining in the specified
area.  Then, find the length of the shortest path between A' and B',
while flying at that constant elevation f_(min).

Give that length as your answer, rounded to three decimal places.

Note: For convenience, the elevation function shown above is repeated
below, in a form suitable for most programming languages:

h=( 5000-0.005*(x*x+y*y+x*y)+12.5*(x+y) ) * exp( -abs(0.000001*(x*x+y*y)-0.0015*(x+y)+0.7) )


---------------------------------------------------------------------}

type R = Double

f1 :: (R, R) -> R
f1 (x, y) = 5000 - 0.005*(x*x+y*y+x*y) + 12.5*(x+y)

f2 :: (R, R) -> R
f2 (x, y) = 0.000001*(x*x + y*y) - 0.0015*(x+y) + 0.7

height :: (R, R) -> R
height (x, y) = f1(x,y) * exp(-abs(f2(x,y)))

{---------------------------------------------------------------------
gnuplot commands:

gnuplot> set contour both
gnuplot> set isosamples 33
gnuplot> f(x,y) = ( 5000-0.005*(x*x+y*y+x*y)+12.5*(x+y) )               
gnuplot> g(x,y) = (0.000001*(x*x+y*y)-0.0015*(x+y)+0.7)  
gnuplot> h(x,y) = f(x,y) * exp(-abs(g(x,y)))
gnuplot> splot [0:1600] [0:1600] h(x,y)

Edge x or y=0: maximum is about (895.4, 10395.6)
Edge x or y=1600: maximum near  (666.5, 9571.5)

----------------------------------------------------------------------

Height along side where y=0:

f(x) = 5000 - (x^2)/200 + 25(x)/2
g(x) = (x^2)/1000000 - 3(x)/2000 + 7/10  (always positive!)

f'(x) = -(x)/100 + 25/2
g'(x) = (x)/500000 - 3/2000

h(x) = f(x) * exp(-abs(g(x)))
h(x) = f(x) * exp(-g(x))

D[h(x)] = 0
D[f(x) * exp(-g(x))] = 0
f(x) * D[exp(-g(x))] + D[f(x)] * exp(-g(x)) = 0
f(x) * exp(-g(x)) * D[-g(x)] + (25/2 - x/100) * exp(-g(x)) = 0
f(x) * exp(-g(x)) * (-g'(x)) + f'(x) * exp(-g(x)) = 0

exp(-g(x)) * [ f(x) * (-g'(x)) + f'(x) ] = 0
f(x) * (-g'(x)) + f'(x) = 0
f'(x) - f(x) * g'(x) = 0

(25/2 - x/100) - (5000 - x^2/200 + 25x/2) * (x/500000 - 3/2000) = 0
x^3/100,000,000 - 13x^2/400,000 - x/800 + 20 = 0
x = 895.48340989876

Let X = x/4000
640 X^3 - 520 X^2 - 5 X + 20 = 0







---------------------------------------------------------------------}


xmin :: R
xmin = 895.48340989876

fmin :: R
fmin = height (xmin, 0)

