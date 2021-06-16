module Euler269 where

{---------------------------------------------------------------------
Problem 269
Polynomials with at least one integer root

19 December 2009

A root or zero of a polynomial P(x) is a solution to the equation P(x) = 0.
Define P_(n) as the polynomial whose coefficients are the digits of n.
For example, P_(5703)(x) = 5x^(3) + 7x^(2) + 3.

We can see that:

    * P_(n)(0) is the last digit of n,
    * P_(n)(1) is the sum of the digits of n,
    * P_(n)(10) is n itself.

Define Z(k) as the number of positive integers, n, not exceeding k for which the polynomial P_(n) has at least one integer root.

It can be verified that Z(100 000) is 14696.

What is Z(10^(16))?

---------------------------------------------------------------------}



{---------------------------------------------------------------------

Since all the coefficients are non-negative, then for n and x positive,
P_(n)(x) is always strictly positive.  Thus the roots cannot be positive.

Also, P_(n)(x) == P_(n)(0) (mod x).
Thus, P_(n)(x) = 0 --> x | P_(n)(0).
That is, if x is a root, then x divides the least-significant digit of n.

 0 is a root iff 10 | n.
-1 is a root iff 11 | n.
-2 is a root --> n == [2,4,6,8] (mod 10)
-3 is a root --> n == [3,6,9] (mod 10)
-4 is a root --> n == 4 or 8 (mod 10)
-5 is a root --> n == 5 (mod 10)
-6 is a root --> n == 6 (mod 10)
-7 is a root --> n == 7 (mod 10)
-8 is a root --> n == 8 (mod 10)
-9 is a root --> n <- [19, 1919, 191919, 19191919, 1919191919, ...


-8 is a root: n = (19*8)+
-7 is a root: n = (18*7)+
-6 is a root: n = (17*6)+
-5 is a root: n = (16*5)+
-4 is a root: (15*(4|68)|28)* (are there more?)
-3 is a root: 13, 26, 39 ...

---------------------------------------------------------------------}
