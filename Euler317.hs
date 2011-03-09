module Euler317 where
import EulerLib (showFloat)

{-

Problem 317
01 January 2011

A firecracker explodes at a height of 100 m above level ground. It
breaks into a large number of very small fragments, which move in
every direction; all of them have the same initial velocity of 20 m/s.

We assume that the fragments move without air resistance, in a uniform
gravitational field with g=9.81 m/s^2.

Find the volume (in m^3) of the region through which the fragments
move before reaching the ground. Give your answer rounded to four
decimal places.

-}


{-

PE = mgh
KE = 1/2 mv^2

Initial energy per unit mass:
E/m = (9.81 m/s^2)(100m) + 1/2 (20m/s)^2
    = 981 m^2/s^2 + 200 m^2/s^2
    = 1181 m^2/s^2

Maximum possible height hmax = 1181/9.81 ~ 120.39 m
hmax = h + v^2/2g

Radius at h=100 m: r_100 = 2 * 20.39 m

Radius at original height = r_h = v^2/g

z(r) = hmax - k*r^2
z(v^2/g) = h
hmax - k*(v^2/g)^2 = h
hmax - k*v^4/g^2 = h
h + (v^2)/(2*g) - k*v^4/g^2 = h
(v^2)/(2*g) - k*v^4/g^2 = 0
(v^2)/(2*g) = k*v^4/g^2
gv^2/2 = k*v^4
g/2v^2 = k
z(r) = hmax - (g/2v^2)*r^2

Where will the farthest pieces hit the ground?

z(r) = 0
hmax - (g/2v^2)*r^2 = 0
(g/2v^2)*r^2 = hmax
(g/2v^2)*r^2 = h + v^2/(2*g)
(g/v^2)*r^2 = 2h + v^2/g
gr^2 = 2hv^2 + v^4/g
r^2 = 2hv^2/g + v^4/g^2

What is the volume of the paraboloid?

volume = pi/2 * r^2 * hmax
volume = pi/2 * (2hv^2/g + v^4/g^2) * hmax
volume = pi/2 * (2hv^2/g + v^4/g^2) * (h + v^2/(2*g))

Let s = v^2/g (dimensions of distance)

volume = pi/2 * (2hs + s^2) * (h + s/2)
volume = pi/2 * s * (2h + s) * (h + s/2)
volume = pi * s * (h + s/2) * (h + s/2)
volume = pi * s * (h + s/2)^2

-}

prob317 h v g = pi * s * (h + s/2)^2
  where s = v^2 / g

main :: IO String
main = return $ showFloat 4 $ prob317 100 20 9.81

answer :: String
answer = "1856532.8455"
