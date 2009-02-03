module Euler154 where
import Data.Array.IArray
import Data.Array.Unboxed ( UArray )

{-
Problem 154
Pascal's pyramid.

12 May 2007

A triangular pyramid is constructed using spherical balls so that each
ball rests on exactly three balls of the next lower level.

Then, we calculate the number of paths leading from the apex to each
position: A path starts at the apex and progresses downwards to any of
the three spheres directly below the current position.

Consequently, the number of paths to reach a certain position is the
sum of the numbers immediately above it (depending on the position,
there are up to three numbers above it).

The result is Pascal's pyramid and the numbers at each level n are the
coefficients of the trinomial expansion (x + y + z)^(n).

How many coefficients in the expansion of (x + y + z)^(200000) are
multiples of 10^(12)?
-}

-- See also Problem 148.

exponent_in_factorial p n =
  if n == 0 then 0 else
  let m = n `div` p in m + exponent_in_factorial p m

{-
for a+b+c = n,
coefficient of (x^a y^b z^c) in (x+y+z)^n is
P(a,b,c) = (a+b+c)! / (a! b! c!)

Pascal's triangle version:
P(a,b) = (a+b)! / (a! b!)

Pyramid function in terms of triangle function:
P(a,b,c) = P(a,b+c) * P(b,c)

200000 in base 5: 22400000

The exponent of 5 in P(a,b,c) equals the total
number of carries in a+b+c, written in base 5.

-}

-- How many values in level m of the pyramid are multiples of 10^k?
prob154 :: Int -> Int -> Int
prob154 m k = sum
  [ r |
    -- only consider a >= b >= c
    a <- [(m + 2) `div` 3 .. m],
    let d2' = m2 - e2!a,
    let d5' = m5 - e5!a,
    max_carries2 5 (m-a) + (m5 - e5!a - e5!(m-a)) >= k,
    max_carries2 2 (m-a) + (m2 - e2!a - e2!(m-a)) >= k,
    b <- [(m - a + 1) `div` 2 .. min a (m-a)],
    let c = m - a - b,
--    if c < 0 then error (show (a,b,c)) else True,
    let d5 = d5' - e5!b - e5!c,
    d5 >= k,
    let d2 = d2' - e2!b - e2!c,
    d2 >= k,
    let r = if a == b || b == c then 3 else 6 ]
  where
    e2, e5 :: UArray Int Int
    e2 = listArray (0, m) $ map (exponent_in_factorial 2) [0 .. m]
    e5 = listArray (0, m) $ map (exponent_in_factorial 5) [0 .. m]
    m2 = exponent_in_factorial 2 m
    m5 = exponent_in_factorial 5 m

-- maximum number of carries for 2 numbers in base b, totalling n.
max_carries2 :: Int -> Int -> Int
max_carries2 b n = f n 0
  where
    f n c
      | q < c = q
      | c <= q = c' + f q c'
      where
        (q, r) = divMod n b
        c' = (2*(b-1) - r + c) `div` b

main :: IO String
main = return $ show $ prob154 200000 12

answer :: String
answer = "479742450"
