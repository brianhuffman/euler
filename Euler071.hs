module Euler071 where
import EulerLib
import Primes

------------------------------------------------------------------------------
-- 71. Listing reduced proper fractions in ascending order of size.
{-
Consider the fraction, n/d, where n and d are positive integers. If n < d and
HCF(n,d)=1, it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d 8 in ascending order of
size, we get:

1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2,
4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that 2/5 is the fraction immediately to the left of 3/7.

By listing the set of reduced proper fractions for d 1,000,000 in ascending
order of size, find the numerator of the fraction immediately to the left of
3/7.
-}

{-
Task: Find greatest c/d < a/b, for d <= n.

For any given d, the optimal value of c is given by:
  c = (d * a) `div` b.

Law about div and mod:
  (x div y)*y = x - x mod y

We want to minimize (a/b - c/d).
= (a*d - b*c) / (b*d)
= (a*d - b*((a*d) div b)) / (b*d)
= (a*d - (a*d - a*d mod b)) / (b*d)
= (a*d mod b) / (b*d)

We should have (a*d mod b) = 1
i.e.  a*d == 1  (mod b)
i.e. d is the inverse of a, mod b.
-}

type Frac = (Int, Int)

prob71 :: Frac -> Int -> Frac
prob71 (a, b) n = (c', d')
  where
    m = invMod a b
    -- greatest d <= n, equivalent to m (mod b)
    d = n - (n `mod` b - m) `mod` b
    c = d * a `div` b
    d' = d `div` gcd c d
    c' = c `div` gcd c d

main :: IO String
main = return $ show $ fst $ prob71 (3, 7) (10^6)
-- 428570
