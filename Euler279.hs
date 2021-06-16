module Euler279 where
import Pythagorean

{---------------------------------------------------------------------
Problem 279
20 February 2010

How many triangles are there with integral sides, at least one
integral angle (measured in degrees), and a perimeter that does not
exceed 10^(8)?

Clarification: Reflections are considered to be the same triangle.

---------------------------------------------------------------------}


{---------------------------------------------------------------------

Law of cosines:

c^2 = a^2 + b^2 - 2ab cos(C)

2ab cos(C) = a^2 + b^2 - c^2

cos(C) = (a^2 + b^2 - c^2) / 2ab

cos(C) must be rational.

The only possible values of C are: 60, 90, 120 degrees

---------------------------------------------------------------------}

count_90_upto m =
  sum [ m `div` (a+b+c) | (a,b,c) <- primitive_pythagorean_triples m ]

count_120_upto m =
  sum [ m `div` (a+b+c) | (a,b,c) <- primitive_torricelli_triples m ]

