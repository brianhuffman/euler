module Euler157 where
import Primes

{-
Problem 157

01 June 2007

Consider the diophantine equation 1/a + 1/b = p/10^n with a, b, p, n
positive integers and a <= b.

For n = 1 this equation has 20 solutions that are listed below:
1/1 + 1/1 = 20/10
1/1 + 1/2 = 15/10
1/1 + 1/5 = 12/10
1/1 + 1/10 = 11/10
1/2 + 1/2 = 10/10
1/2 + 1/5 = 7/10
1/2 + 1/10 = 6/10
1/3 + 1/6 = 5/10
1/3 + 1/15 = 4/10
1/4 + 1/4 = 5/10
1/4 + 1/20 = 3/10
1/5 + 1/5 = 4/10
1/5 + 1/10 = 3/10
1/6 + 1/30 = 2/10
1/10 + 1/10 = 2/10
1/11 + 1/110 = 1/10
1/12 + 1/60 = 1/10
1/14 + 1/35 = 1/10
1/15 + 1/30 = 1/10
1/20 + 1/20 = 1/10

How many solutions has this equation for 1 <= n <= 9? 
-}

{-
let M = 10^n
1/a + 1/b = p/M
M/a + M/b = p
(a+b)M = abp
ab divides (a+b)M

if gcd a b = c:
  a = cx, b = cy
  (a+b)M = abp
  (cx+cy)M = (cx)(cy)p
  (x+y)M = xy(cp)

consider only solutions where gcd a b == 1

ab divides (a+b)M
a divides (a+b)M   AND      b divides (a+b)M
a divides bM       AND      b divides aM
a divides M       AND      b divides M

coprime (a,b) where a,b both divide 10^n:
  [ (1, 2^i * 5^j) | i <- [0..n], j <- [0..n] ]
  [ (2^i, 5^j) | i <- [1..n], j <- [1..n] ]

for n=1, factors (10^1) = [1,2,5,10]
(1,1,20) * [1,2,4,5,10,20]
(1,2,15) * [1,3,5,15]
(1,5,12) * [1,2,3,4,6,12]
(1,10,11) * [1,11]
(2,5,7) * [1,7]
total = 20
-}

num_solutions n = sum [ num_divisors p | (a,b,p) <- abp1 ++ abp2 ]
  where
    f i j = 2^i * 5^j
    abp1 = [ (1, f i j, 10^n + f (n-i) (n-j)) | i <- [0..n], j <- [0..n] ]
    abp2 = [ (2^i, 5^j, f (n-i) n + f n (n-j)) | i <- [1..n], j <- [1..n] ]

prob157 m = sum [ num_solutions n | n <- [1 .. m] ]

main :: IO String
main = return $ show $ prob157 9
-- 53490
