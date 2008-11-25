module Euler169 where

{-
Problem 169
23 November 2007

Define f(0)=1 and f(n) to be the number of different ways n can be expressed
as a sum of integer powers of 2 using each power no more than twice.

For example, f(10)=5 since there are five different ways to express 10:

1 + 1 + 8
1 + 1 + 4 + 4
1 + 1 + 2 + 2 + 4
2 + 4 + 4
2 + 8

What is f(10^25)?
-}

{-
f(0) = 1: 0
f(1) = 1: 1
f(2) = 2: 10, 02
f(3) = 1: 11
f(4) = 3: 100, 020, 012
f(5) = 2: 101, 021
f(6) = 3: 110, 102, 022
f(10) = 5: 1002, 0202, 0122, 0210, 1010

In a representation of 2*n, 1 may appear either zero or two times.
If 1 does not appear, then the remaining terms are a representation
  of n, but with each term doubled.
If 1 appears twice, then the remaining terms are a representation
  of n-1, but with each term doubled.

In a representation of 2*n+1, 1 must appear exactly once.
The remaining terms are a representation of n, with each term doubled.

f(0) = 1
f(2n+1) = f(n)
f(2n+2) = f(n+1) + f(n)

f(2n-1) = f(n-1)
f(2n) = f(n) + f(n-1)
f(2n+1) = f(n)

f(4n) = f(2n) + f(2n-1) = f(n) + 2*f(n-1)
f(8n) = f(2n) + 2*f(2n-1) = f(n) + 3*f(n-1)
-}

ways 0 = 1
ways n = case divMod n 2 of
  (q, 0) -> ways q + ways (q-1)
  (q, 1) -> ways q

-- ways2 n = (ways n, ways (n-1))
ways2 1 = (1, 1)
ways2 n = if even n then (x+y, y) else (x, x+y)
  where (x, y) = ways2 (n `div` 2)

main :: IO String
main = return $ show $ fst $ ways2 (10^25)
-- 178653872807

ways2_mod m 1 = (1, 1)
ways2_mod m n = if even n then ((x+y) `mod` m, y) else (x, (x+y) `mod` m)
  where (x, y) = ways2_mod m (n `div` 2)

