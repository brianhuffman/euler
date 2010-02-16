module Euler278 where
import Primes

{---------------------------------------------------------------------
Problem 278
Linear Combinations of Semiprimes

13 February 2010

Given the values of integers 1 < a_(1) < a_(2) <... < a_(n), consider
the linear combination q_(1)a_(1) + q_(2)a_(2) + ... + q_(n)a_(n) = b,
using only integer values q_(k) ≥ 0.

Note that for a given set of a_(k), it may be that not all values of b
are possible.  For instance, if a_(1) = 5 and a_(2) = 7, there are no
q_(1) ≥ 0 and q_(2) ≥ 0 such that b could be 1, 2, 3, 4, 6, 8, 9, 11,
13, 16, 18 or 23.  In fact, 23 is the largest impossible value of b
for a_(1) = 5 and a_(2) = 7.  We therefore call f(5, 7) = 23.
Similarly, it can be shown that f(6, 10, 15) = 29 and f(14, 22, 77) =
195.

Find ∑ f(p*q,p*r,q*r), where p, q and r are prime numbers and p < q <
r < 5000.

---------------------------------------------------------------------}

{---------------------------------------------------------------------

For coprime a, b:
f(a,b) = (b-1)a + (a-1)b - ab = ab-a-b+1

-------------------------------------------------

7 cannot be written as a sum of 3s and 5s.

7/3 == 4 (mod 5)
7/5 == 2 (mod 3)

All 15 points are distinct (mod 15):

          11111111112
012345678901234567890
*    *    *    |
   *    *    * |
 ?    *    *   |*
    ?    *    *|   *
  ?    ?    *  | *    *

Each gap corresponds to some point > 15.
The rightmost gap corresponds to a(b-1) + (a-1)b.

-----------------------------------------------


f(6,10,15)


          111111111122222222223333333333444444444455555555556
0123456789012345678901234567890123456789012345678901234567890
*         *         *         |                             |
      *         *         *   |                             |
  ?         *         *       | *                           |
        ?         *         * |       *                     |
    ?         ?         *     |   *         *               |
------------------------------|-----------------------------|
     ?         *         *    |    *                        |
 ?         ?         *        |*         *                  |
       ?         ?         *  |      *         *            |
   ?         ?         ?      |  *         *         *      |
         ?         ?         ?|        *         *         *|


---------------------------------------------------------------------}

type Z = Integer

gap2 :: Z -> Z -> Z
gap2 a b = a*b - a - b + 1

gap3 :: Z -> Z -> Z -> Z
gap3 a b c = 2*a*b*c - a*b - a*c - b*c
-- (c-1)*a*b + (b-1)*a*c + (a-1)*b*c - a*b*c

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (\y -> (x,y)) xs ++ pairs xs

triples :: [a] -> [(a, a, a)]
triples [] = []
triples (x : xs) = map (\(y,z) -> (x,y,z)) (pairs xs) ++ triples xs

prob278 :: [Z] -> Z
prob278 xs = sum [ gap3 a b c | (a,b,c) <- triples xs ]

main :: IO String
main = return $ show $ prob278 $ takeWhile (<5000) primes

answer :: String
answer = "1228215747273908452"
