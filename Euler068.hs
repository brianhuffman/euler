module Euler068 where
import Permutation
import Data.List

{-
Problem 68
What is the maximum 16-digit string for a "magic" 5-gon ring?

23 April 2004

Consider the following "magic" 3-gon ring, filled with the numbers 1 to 6, and
each line adding to nine.

   4
    \
     3
    / \
   1 - 2 - 6
  /
 5

Working clockwise, and starting from the group of three with the numerically
lowest external node (4,3,2 in this example), each solution can be described
uniquely. For example, the above solution can be described by the set: 4,3,2;
6,2,1; 5,1,3.

It is possible to complete the ring with four different totals: 9, 10, 11, and
12. There are eight solutions in total.

Total	Solution Set
9	4,2,3; 5,3,1; 6,1,2
9	4,3,2; 6,2,1; 5,1,3
10	2,3,5; 4,5,1; 6,1,3
10	2,5,3; 6,3,1; 4,1,5
11	1,4,6; 3,6,2; 5,2,4
11	1,6,4; 5,4,2; 3,2,6
12	1,5,6; 2,6,4; 3,4,5
12	1,6,5; 3,5,4; 2,4,6

By concatenating each group it is possible to form 9-digit strings; the maximum
string for a 3-gon ring is 432621513.

Using the numbers 1 to 10, and depending on arrangements, it is possible to
form 16- and 17-digit strings. What is the maximum 16-digit string for a
"magic" 5-gon ring?
-}

{-
Analysis:

If the 10 appears on an outer node, the string has length 16.
If the 10 appears on an inner node, the string has length 17.

Thus the 10 must appear on an outer node.

The digit string starts with the lowest-valued outer node.
To maximize this, we should have all the highest values on
the outside: 6, 7, 8, 9, 10. Thus the string starts with 6.

The sum of each line is equal to
(sum_outer + 2 * sum_inner) / 5 = (40 + 2*15) / 5 = 70 / 5 = 14.

The second symbol of the string is the value next to the 6;
this should be the largest value remaining, which is 5.

This means that to add to 14, the third value needs to be 3.
Row 1: 6,5,3.

Then row 2 has a 3 in the middle.
-}
-- 6,5,3;  10,3,1;  9,1,4;  8,4,2;  7,2,5

magic_5_gons :: [String]
magic_5_gons =
  [ concatMap show [x1,y1,y2, x2,y2,y3, x3,y3,y4, x4,y4,y5, x5,y5,y1] |
    inner5 <- subseqs_len 5 [1 .. 10],
    let outer5 = [1 .. 10] \\ inner5,
    sum inner5 `mod` 5 == 0,
    let row_sum = (sum outer5 + 2 * sum inner5) `div` 5,
    (x1, outer4) <- remove1 outer5,
    x1 == minimum outer5,
    (y1, inner4) <- remove1 inner5,
    (y2, inner3) <- remove1 inner4,
    x1 + y1 + y2 == row_sum,
    (x2, outer3) <- remove1 outer4,
    (y3, inner2) <- remove1 inner3,
    x2 + y2 + y3 == row_sum,
    (x3, outer2) <- remove1 outer3,
    (y4, inner1) <- remove1 inner2,
    x3 + y3 + y4 == row_sum,
    (x4, outer1) <- remove1 outer2,
    (y5, inner0) <- remove1 inner1,
    x4 + y4 + y5 == row_sum,
    (x5, outer0) <- remove1 outer1,
    x5 + y5 + y1 == row_sum ]

main :: IO String
main = return $ maximum magic_5_gons
-- 6531031914842725
