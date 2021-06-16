module Euler264 where

{---------------------------------------------------------------------
Problem 264
Triangle Centres

Consider all the triangles having:

    * All their vertices on lattice points.
    * Circumcentre at the origin O.
    * Orthocentre at the point H(5, 0).

There are nine such triangles having a perimeter ≤ 50.
Listed and shown in ascending order of their perimeter, they are:

A(-4, 3), B(5, 0), C(4, -3)
A(4, 3), B(5, 0), C(-4, -3)
A(-3, 4), B(5, 0), C(3, -4)


A(3, 4), B(5, 0), C(-3, -4)
A(0, 5), B(5, 0), C(0, -5)
A(1, 8), B(8, -1), C(-4, -7)


A(8, 1), B(1, -8), C(-4, 7)
A(2, 9), B(9, -2), C(-6, -7)
A(9, 2), B(2, -9), C(-6, 7)
	

The sum of their perimeters, rounded to four decimal places, is
291.0089.

Find all such triangles with a perimeter ≤ 10^(5).  Enter as your
answer the sum of their perimeters rounded to four decimal places.

---------------------------------------------------------------------}


{---------------------------------------------------------------------

Solution has the form (a,b) (c,d) (e,f), all integers.

Circumcentre at the origin means:

  a^2 + b^2 = c^2 + d^2 = e^2 + f^2

Orthocentre at (x,y) means:

cross ((a,b) - (c,d)) ((e,f) - (x,y)) = 0
cross (a-c, b-d) (e-x, f-y) = 0
(a-c)(f-y) - (b-d)(e-x) = 0
(a-c)f - (a-c)y - (b-d)e + (b-d)x = 0
af - ay - cf + cy - be + bx + de - dx = 0

(c,d) <-> (e,f)

(b-d)x + (c-a)y + (af-cf-be+de) = 0
(c,d) <-> (e,f)
(b-f)x + (e-a)y + (ad-ed-bc+fc) = 0


(b-d)x + (c-a)y + (af-cf-be+de) = 0
(b-f)x + (e-a)y + (ad-ed-bc+fc) = 0

(b-d)(b-f)x + (b-f)(c-a)y + (b-f)(af-cf-be+de) = 0
(b-d)(b-f)x + (b-d)(e-a)y + (b-d)(ad-ed-bc+cf) = 0

(b-d)(b-f)x + (bc-ab-cf+af)y + (abf-bcf-bbe+bde-aff+cff+bef-def) = 0
(b-d)(b-f)x + (be-ab-de+ad)y + (abd-bde-bbc+bcf-add+edd+bcd-cdf) = 0

(bc-ab-cf+af-be+ab+de-ad)y +
(abf-bcf-bbe+bde-aff+cff+bef-def-abd+bde+bbc-bcf+add-edd-bcd+cdf) = 0

(bc-cf+af-be+de-ad)y +
(abf-2bcf-bbe+2bde-aff+cff+bef-def-abd+bbc+add-edd-bcd+cdf) = 0




ad - ay - ed + ey - bc + bx + fc - fx = 0

af - ay - cf + cy - be + bx + de - dx = 0


ad - ay - ed + ey - bc + bx + fc - fx = 0



(a-c)f - (a-c)y - (b-d)e + (b-d)x = 0
(a-e)d - (a-e)y - (b-f)c + (b-f)x = 0

(x,y) -> (5,0)

(a-c)f - (b-d)e + (b-d)5 = 0
(a-e)d - (b-f)c + (b-f)5 = 0

---------------------------------------------------------------------}