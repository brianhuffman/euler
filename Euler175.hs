module Euler175 where
import List (intersperse)

{-
Problem 175

28 December 2007

Define f(0)=1 and f(n) to be the number of ways to write n as a sum of powers
of 2 where no power occurs more than twice.

For example, f(10)=5 since there are five different ways to express 10:
10 = 8+2 = 8+1+1 = 4+4+2 = 4+2+2+1+1 = 4+4+1+1

It can be shown that for every fraction p/q (p>0, q>0) there exists at least
one integer n such that
f(n)/f(n-1)=p/q.

For instance, the smallest n for which f(n)/f(n-1)=13/17 is 241.
The binary expansion of 241 is 11110001.
Reading this binary number from the most significant bit to the least
significant bit there are 4 one's, 3 zeroes and 1 one. We shall call the
string 4,3,1 the Shortened Binary Expansion of 241.

Find the Shortened Binary Expansion of the smallest n for which
f(n)/f(n-1) = 123456789/987654321.

Give your answer as comma separated integers, without any whitespaces.
-}

{-
See also Problem 169.

Define g(n) = (f(n), f(n-1)).
Define n;0 = 2n
Define n;1 = 2n+1

g(1) = (1, 1)
g(2n+0) = (x+y, y) where (x, y) = g(n)
g(2n+1) = (x, x+y) where (x, y) = g(n)

g(n followed by k 0s) = (x+ky, y) where (x, y) = g(n)
g(n followed by k 1s) = (x, y+kx) where (x, y) = g(n)
-}

prob175a x y
  | x < y = let (q, r) = divMod y x
            in if r == 0 then [q] else q : prob175a x r
  | y < x = let (q, r) = divMod x y
            in if r == 0 then [q-1,1] else q : prob175a r y
  | otherwise = []

prob175 a b = reverse $ prob175a a b

showall = concat . intersperse "," . map show

main :: IO String
main = return $ showall $ prob175 123456789 987654321
-- 1,13717420,8
