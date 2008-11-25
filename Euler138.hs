module Euler138 where
import Pythagorean

------------------------------------------------------------------------------
-- 138. Investigating isosceles triangle for which the height and base length differ by one.
{-
Consider the isosceles triangle with base length, b = 16, and legs, L = 17.

    /|\
   / | \
 L/  |  \L
 /  h|   \
/    |    \
-----------
     b

By using the Pythagorean theorem it can be seen that the height of the
triangle, h = (17^2 - 8^2) = 15, which is one less than the base length.

With b = 272 and L = 305, we get h = 273, which is one more than the base
length, and this is the second smallest isosceles triangle with the property
that h = b +/- 1.

Find SUM L for the twelve smallest isosceles triangles for which h = b +/- 1
and b, L are positive integers.
-}

{-
Analysis:

Solutions are given by pythagorean triples (a,h,L) with
h = 2a+1 or h = 2a-1.

Case h = 2a+1:
a^2 + h^2 = L^2
a^2 + (2a+1)^2 = c^2
5a^2 + 4a + 1 = L^2

Case h = 2a-1:
a^2 + h^2 = L^2
a^2 + (2a-1)^2 = L^2
5a^2 - 4a + 1 = L^2

Integer solutions to 5a^2 + 4a + 1 = L^2
                  or 5a^2 - 4a + 1 = L^2
(a, L)
------
+ (0, 1)
- (8, 17)
+ (136, 305)
- (2448, 5473)
+ (43920, 98209)
- (788120, 1762289)
+ (14142232, 31622993)
- (253772064, 567451585)

a' = -(9a + 4l + 4)
l' = -(20a + 9l + 8)
-}

find_solutions n =
  filter (\(a,b,c) -> b `elem` [2*a-1, 2*a+1]) $
  primitive_pythagorean_triples n

leg_lengths :: [Integer]
leg_lengths = drop 1 (f 0 1)
  where f a l = abs l : f (-9*a - 4*l - 4) (-20*a - 9*l - 8)

{-
prob138b = f 136 305
  where f a c = c : f (161*a + 72*c + 64) (360*a + 161*c + 144)

prob138c = f 8 17
  where f a c = c : f (161*a + 72*c - 64) (360*a + 161*c - 144)
-}

prob138 :: Int -> Integer
prob138 n = sum (take n leg_lengths)

main :: IO String
main = return $ show $ prob138 12
-- 1118049290473932
