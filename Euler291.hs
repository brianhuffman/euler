module Euler291 where
import Primes

{-

Problem 291
Panaitopol Primes
07 May 2010

A prime number p is called a Panaitopol prime if

    x^4 - y^4
p = ---------
    x^3 + y^3

for some positive integers x and y.

Find how many Panaitopol primes are less than 5×10^15.

-}


{-
         x^4 - y^4
f(x,y) = ---------
         x^3 + y^3

Note that f is linear: f(kx, ky) = k f(x,y)
So if f(x,y) equals the reduced fraction a/b,
then f(b*x, b*y) will yield the integer a.

Factorize:
(x^4 - y^4) = (x^2 + y^2)(x + y)(x - y)
(x^3 + y^3) = (x + y)(x^2 - xy + y^2)

         (x^2 + y^2)(x - y)
f(x,y) = ------------------
          (x^2 + y^2 - xy)

Let z = x-y, so x=y+z.

           (2y^2 + 2yz + z^2)z
f(y+z,y) = -------------------
             (y^2 + yz + z^2)

If y and z are coprime, then z will not divide the denominator:
y^2 + yz + z^2 == y^2 /= 0 (mod z)

Thus in reduced form, z must divide the numerator of f(y+z,y)

So the numerator can only be prime if z = 1.

           (2y^2 + 2y + 1)
f(y+1,y) = ---------------
            (y^2 + y + 1)

Note that the numerator is equal to twice the denominator minus 1:
a(y) = 2y^2 + 2y + 1
b(y) = y^2 + y + 1

a(y) = 2b(y) - 1

Thus the fraction a(y)/b(y) must already be in reduced form.

Conclusion: a Panaipotol prime is one of the form
p = 2y^2 + 2y + 1, for some integer y.

-}

a :: Integer -> Integer
a y = 2*y^2 + 2*y + 1

panaipotols :: Integer -> [Integer]
panaipotols pmax =
  filter miller_rabin (takeWhile (<pmax) (map a [1..]))

prob291a :: Integer -> Int
prob291a pmax = length (panaipotols pmax)

-- prob291a (10^11) = 26516  (8 s)
-- prob291a (10^12) = 76334  (25 s)
-- prob291a (10^13) = 221763  (86 s)
-- prob291a (10^14) = 645783  (7.5 min)
-- time scales mostly linearly with result
-- miller_rabin takes much more time when argument is prime

{-

For which y can a(y) = 2y^2 + 2y + 1 be prime?

a(y) is always odd.
a(y) is never a multiple of 3.
a(y) is a multiple of 5 if y == 1,3 (mod 5)
a(y) is never a multiple of 7.

a(1) = 5. Thus 5 | a(y) if y == 1 (mod 5).
a(2) = 13. Thus 13 | a(y) if y == 2 (mod 13).
a(3) = 25. Thus 5 | a(y) if y == 3 (mod 5), 5 as largest prime factor of 25.
a(4) = 41. Thus 41 | a(y) if y == 4 (mod 41).
a(5) = 61. Thus 61 | a(y) if y == 5 (mod 61).
a(6) = 85. Thus 17 | a(y) if y == 6 (mod 17), 17 as largest prime factor of 85.
...

-}

main :: IO String
main = return $ show $ prob291a (5*10^15)

answer :: String
answer = "4037526"
