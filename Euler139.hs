module Euler139 where
import EulerLib
import Pythagorean

------------------------------------------------------------------------------
-- 139. Finding Pythagorean triangles which allow the square on the hypotenuse square to be tiled.
{-
Let (a, b, c) represent the three sides of a right angle triangle with integral
length sides. It is possible to place four such triangles together to form a
square with length c.

For example, (3, 4, 5) triangles can be placed together to form a 5 by 5 square
with a 1 by 1 hole in the middle and it can be seen that the 5 by 5 square can
be tiled with twenty-five 1 by 1 squares.

However, if (5, 12, 13) triangles were used then the hole would measure 7 by 7
and these could not be used to tile the 13 by 13 square.

Given that the perimeter of the right triangle is less than one-hundred million,
how many Pythagorean triangles would allow such a tiling to take place?
-}

{-
Analysis:
We need to find solutions to
  a^2 + b^2 = c^2,  where  (b-a) divides c.

If we find a non-primitive solution, where k = gcd(a,b,c) > 1,
Then (a/k, b/k, c/k) is also a solution.

Thus we only need to consider primitive solutions, where
a, b, and c are all coprime.

Primitive triples take the form (2*r*s, s^2 - r^2, s^2 + r^2),
where r and s are coprime.

We want to show that (b-a) = 1.

By contradiction, we will assume that (b-a) > 1, so that it has
a prime factor p.

Note that for primitive triples, b-a is always odd, so p > 2.

p | b-a | c
p | c+(b-a)
p | c-(b-a)
p | (s^2 + r^2) + (s^2 - r^2) - 2rs
p | 2s^2 - 2rs
p | 2s(s-r)
p | s(s-r)
p | (s^2 + r^2) - (s^2 - r^2) + 2rs
p | 2r^2 + 2rs
p | 2r(r+s)
p | r(r+s)

p | s(s-r)  <-->  (p | s)  OR  (p | s-r).
p | r(r+s)  <-->  (p | r)  OR  (p | r+s).

For any combination, we can show that p | r and p | s,
contradicting the fact that r and s are coprime.
Thus b-a = 1.

----------------------------------------------------------

Now the problem amounts to finding primitive triples (a,b,c)
such that b = a+1.

a^2 + b^2 = c^2
a^2 + (a+1)^2 = c^2
a^2 + (a^2 + 2a + 1) = c^2
2a^2 + 2a + 1 = c^2

Solutions to 2a^2 + 2a + 1 = c^2
(a, c)
------
(3, 5)
(20, 29)
(119, 169)
(696, 985)
(4059, 5741)
(23660, 33461)
(137903, 195025)
(803760, 1136689)
(4684659, 6625109)
(27304196, 38613965)

Recurrence equations:
a' = 3a + 2c + 1
c' = 4a + 3c + 2
-}

find_primitive_solutions :: Integer -> [(Integer, Integer, Integer)]
find_primitive_solutions m =
  filter (\(a,b,c) -> divides (b-a) c) $
  primitive_pythagorean_triples m

primitive_perimeters :: [Integer]
primitive_perimeters = f 3 5
  where
    f a c = 2*a + 1 + c : f (3*a + 2*c + 1) (4*a + 3*c + 2)

prob139 :: Integer -> Integer
prob139 m = sum
  [ m `div` l | l <- takeWhile (<m) primitive_perimeters ]

main :: IO String
main = return $ show $ prob139 (10^8)
-- 10057761
