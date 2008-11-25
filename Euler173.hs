module Euler173 where

{-
Problem 173
22 December 2007

We shall define a square lamina to be a square outline with a square "hole" so
that the shape possesses vertical and horizontal symmetry. For example, using
exactly thirty-two square tiles we can form two different square laminae:

XXXXXX  XXXXXXXXX
XXXXXX  X       X
XX  XX  X       X
XX  XX  X       X
XXXXXX  X       X
XXXXXX  X       X
        X       X
        X       X
        XXXXXXXXX

With one-hundred tiles, and not necessarily using all of the tiles at one time,
it is possible to form forty-one different square laminae.

Using up to one million tiles how many different square laminae can be formed?
-}

{-
inner square size a, outer square size b
a == b  (mod 2),  i.e. (b - a) is even
b-a = 2d, where d = thickness
b = a + 2d

number of tiles
= b^2 - a^2
= (a+2d)^2 - a^2
= (a^2 + 4da + 4d^2) - a^2
= 4 (da + d^2)
= 4d (a + d)

if 4d (a + d) <= n,
then (a + d) can range from d+1 .. n`div`(4*d)
i.e. a can range from 1 .. n`div`(4*d)-d
-}

prob173 n =
  sum $ takeWhile (> 0) $
  [ n `div` (4*d) - d | d <- [1 ..] ]

main :: IO String
main = return $ show $ prob173 (10^6)
-- 1572729
