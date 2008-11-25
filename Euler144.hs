module Euler144 where

------------------------------------------------------------------------------
-- 144. Investigating multiple reflections of a laser beam.
{-
In laser physics, a "white cell" is a mirror system that acts as a delay line
for the laser beam. The beam enters the cell, bounces around on the mirrors,
and eventually works its way back out.

The specific white cell we will be considering is an ellipse with the equation
4x^2 + y^2 = 100

The section corresponding to -0.01 <= x <= +0.01 at the top is missing,
allowing the light to enter and exit through the hole.

The light beam in this problem starts at the point (0.0,10.1) just outside the
white cell, and the beam first impacts the mirror at (1.4,-9.6).

Each time the laser beam hits the surface of the ellipse, it follows the usual
law of reflection "angle of incidence equals angle of reflection." That is,
both the incident and reflected beams make the same angle with the normal line
at the point of incidence.

In the figure on the left, the red line shows the first two points of contact
between the laser beam and the wall of the white cell; the blue line shows the
line tangent to the ellipse at the point of incidence of the first bounce.

The slope m of the tangent line at any point (x,y) of the given ellipse is:
m = -4x/y

The normal line is perpendicular to this tangent line at the point of incidence.

The animation on the right shows the first 10 reflections of the beam.

How many times does the beam hit the internal surface of the white cell before
exiting?
-}

{-
vector (vx,vy) reflected off a surface with tangent (dx,dy)
(dx,dy) = (r*cos t, r*sin t)
rotate right t, invert vertically, rotate left t

rotate left t:
[ +cos -sin ]
[ +sin +cos ]

rotate right t:
[ +cos +sin ]
[ -sin +cos ]

invert vertically:
[ 1   0 ]
[ 0  -1 ]

Full reflection:
[ +cos -sin ]  [ 1   0 ]  [ +cos +sin ]
[ +sin +cos ]  [ 0  -1 ]  [ -sin +cos ]

[ +cos -sin ]  [ +cos +sin ]
[ +sin +cos ]  [ +sin -cos ]

[ cos*cos-sin*sin  sin*cos+sin*cos ]
[ sin*cos+sin*cos  sin*sin-cos*cos ]

[ dx*dx-dy*dy    2*dx*dy   ]
[   2*dx*dy    dy*dy-dx*dx ]

let a = dx*dx-dy*dy
let b = 2*dx*dy

(vx', vy') = (a*vx + b*vy, b*vx - a*vy)

------------------------------
For ellipse 4*x^2 + y^2 = 100:

dx = y1
dy = -4*x1

4*(x+t*h)^2 + (y+t*k)^2 = 4*x^2 + y^2
4*(x^2 + 2*x*t*h + t^2*h^2) + (y^2 + 2*y*t*k + t^2*k^2) = 4*x^2 + y^2
4*(2*x*t*h + t^2*h^2) + (2*y*t*k + t^2*k^2) = 0

4*(2*x*t*h + t^2*h^2) + (2*y*t*k + t^2*k^2) = 0
t^2*(4*h^2 + k^2) + t*(8*x*h + 2*y*k) = 0         (assuming t/=0)
t*(4*h^2 + k^2) + (8*x*h + 2*y*k) = 0
t*(4*h^2 + k^2) = -(8*x*h + 2*y*k)
t = -(8*x*h + 2*y*k)/(4*h^2 + k^2)
-}

type Point = (Double, Double)

next_point :: Point -> Point -> Point
next_point (x0,y0) (x1,y1) = (x2,y2)
  where
    -- (vx,vy) = velocity vector from (x0,y0) to (x1,y1)
    vx = x1 - x0
    vy = y1 - y0
    -- (dx,dy) = tangent vector at (x1,y1)
    dx = y1
    dy = -4 * x1
    -- (vx',vy') = velocity vector from (x1,y1) to (x2,y2)
    a = dx^2 - dy^2
    b = 2*dx*dy
    vx' = a*vx + b*vy
    vy' = b*vx - a*vy
    t = -(8*x1*vx' + 2*y1*vy')/(4*vx'^2 + vy'^2)
    x2 = x1 + t*vx'
    y2 = y1 + t*vy'

points = takeWhile ok $ f (0,10.1) (1.4,-9.6)
  where
    ok (x,y) = y < 0 || abs x > 0.01
    f p0 p1 = p1 : f p1 (next_point p0 p1)

main :: IO String
main = return $ show $ length points
-- 354
