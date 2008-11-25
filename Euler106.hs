module Euler106 where
import EulerLib

------------------------------------------------------------------------------
-- 106. Find the minimum number of comparisons needed to identify special sum sets.
{-
Let S(A) represent the sum of elements in set A of size n. We shall call it a
special sum set if for any two non-empty disjoint subsets, B and C, the
following properties are true:

   1. S(B) S(C); that is, sums of subsets cannot be equal.
   2. If B contains more elements than C then S(B) S(C).

For this problem we shall assume that a given set contains n strictly
increasing elements and it already satisfies the second rule.

Surprisingly, out of the 25 possible subset pairs that can be obtained from a
set for which n = 4, only 1 of these pairs need to be tested for equality
(first rule). Similarly, when n = 7, only 70 out of the 966 subset pairs need
to be tested.

For n = 12, how many of the 261625 subset pairs that can be obtained need to be
tested for equality?

NOTE: This problem is related to problems 103 and 105.
-}

{-
3-elements:
0 < a < b < c, where c < a + b
no comparisons necessary

4-elements:
0 < a < b < c < d
a+d ? b+c
2 vs 2: 1 * 4 choose 4 = 1 comparison

5-elements:
0 < a < b < c < d < e
2 vs 2: 1 * 5 choose 4 = 5 comparisons
a+d ? b+c
a+e ? b+c
a+e ? b+d
a+e ? c+d
b+e ? c+d

6-elements:
0 < a < b < c < d < e < f
2 vs 2: 1 * 6 choose 4 = 15 comparisons
3 vs 3: 5 * 6 choose 6 = 5 comparisons
a+b+c < d+e+f  {{{}}}
a+b+d < c+e+f  {{}{}}
a+b+e < c+d+f  {{}}{}
a+b+f ? c+d+e  {{}}}{
a+c+d < b+e+f  {}{{}}
a+c+e < b+d+f  {}{}{}
a+c+f ? b+d+e  {}{}}{
a+d+e ? b+c+f  {}}{{}
a+d+f ? b+c+e  {}}{}{
a+e+f ? b+c+d  {}}}{{

Partitions that correspond to balanced sets of parentheses
are always ordered (<).

The number of necessary n-to-n comparisons is equal to the total
number of partitions (choose (2*n) n / 2 = choose (2*n-1) n)
minus the number of partitions corresponding to balanced expressions
(catalan n = choose (2*n) n - choose (2*n) (n-1)).

C(n, r) = C(n-1, r-1) + C(n-1, r)

catalan n
= C(2*n, n) - C(2*n, n-1)
= (C(2*n-1, n-1) + C(2*n-1, n)) - (C(2*n-1, n-2) + C(2*n-1, n-1))
= C(2*n-1, n) - C(2*n-1, n-2)

C(2*n-1, n) - catalan n
= C(2*n-1, n) - (C(2*n-1, n) - C(2*n-1, n-2))
= C(2*n-1, n-2)

7-elements:
0 < a < b < c < d < e < f < g
2 vs 2: 7 choose 4 = 35 comparisons
3 vs 3: 5 * 7 choose 6 = 35 comparisons

2 vs 2: 1
3 vs 3: 5
4 vs 4: 21
5 vs 5: 84
6 vs 6: 330
7 vs 7: 1287
-}

prob106 n = sum
  [ choose n (2*k) * choose (2*k-1) (k-2) |
    k <- [2 .. n`div`2] ]

main :: IO String
main = return $ show $ prob106 12
