module Euler103 where
import Permutation
import EulerLib (distinct)

{-
Problem 103
Investigating sets with a special subset sum property.

26 August 2005

Let S(A) represent the sum of elements in set A of size n. We shall call it
a special sum set if for any two non-empty disjoint subsets, B and C, the
following properties are true:

   1. S(B) /= S(C); that is, sums of subsets cannot be equal.
   2. If B contains more elements than C then S(B) > S(C).

If S(A) is minimised for a given n, we shall call it an optimum special sum
set. The first five optimum special sum sets are given below.

n = 1: {1}
n = 2: {1, 2}
n = 3: {2, 3, 4}
n = 4: {3, 5, 6, 7}
n = 5: {6, 9, 11, 12, 13}

It seems that for a given optimum set, A = {a1, a2, ... , an}, the next
optimum set is of the form B = {b, a1+b, a2+b, ... ,an+b}, where b is the
"middle" element on the previous row.

By applying this "rule" we would expect the optimum set for n = 6 to be
A = {11, 17, 20, 22, 23, 24}, with S(A) = 117. However, this is not the
optimum set, as we have merely applied an algorithm to provide a near
optimum set. The optimum set for n = 6 is A = {11, 18, 19, 20, 22, 25},
with S(A) = 115 and corresponding set string: 111819202225.

Given that A is an optimum special sum set for n = 7, find its set string.

NOTE: This problem is related to problems 105 and 106.
-}

{-
Analysis:

Any subset of a special set is also special.
Incrementing all members of a special set gives another special set.


[20,31,38,39,40,42,45]
   [31,38,39,40,42,45]
   [11,18,19,20,22,25]
      [18,19,20,22,25]
      [ 9,10,11,13,16]
      [ 9,10,11,13]
      [ 4, 5, 6, 8]
      [ 4, 5, 6]
      [ 2, 3, 4]
      [ 2, 3]
      [ 1, 2]
      [ 1]




[20,31,38,39,40,42]
[14,25,32,33,34,36]
[14,25,32,33,34]
[11,22,29,30,31]
[11,22,29,30]
[9,20,27,28]
[9,20,27]
[8,19,26]
[8,19]
[1,12]
[1]



-}

diffs :: [Int] -> [Int]
diffs [] = []
diffs (n : ns) = map (subtract n) ns

{-
{a,b,c,d,e,f,g} is special
a < b < c < d < e < f < g
e+f+g < a+b+c+d

34 <= (e+f+g)-(a+b+c) < d

a+b+c+d+e+f+g <= 255
e+f+g <= 127
(e+f+g) - (a+b+c) <= 127 - (a+b+c)

34 <= (e+f+g) - (a+b+c)
20 <= (f+g) - (a+b)

34 < d
-}

special_sum_candidates :: [[Int]]
special_sum_candidates =
  [ [a,b,c,d,e,f,g] |
    d <- [35 .. 61],
    c <- [d-1, d-2 .. d*2`div`3],
    b <- [c-1, c-2 .. d`div`2],
    a <- [b-1, b-2 .. d-b],
    a+d /= b+c,
    e <- [d+1 .. (a+b+c+d)`div`3],
    a+e /= b+c,
    a+e /= c+d,
    b+e /= c+d,
    a+b+c+d+e+e+e < 255,
    f <- [e+1 .. (a+b+c+d-e)`div`2],
    a+b+c+d+e+f+f < 255,
    g <- [f+1 .. (a+b+c+d-e-f-1)],
    a+b+c+d+e+f+g <= 255 ]
    -- e+f+g can range from  a+b+c+34  to a+b+c+d-1
    -- f+g < a+b+c
    -- g < a+b
-- [20,31,38,39,40,42,45]

distinct_subset_sums :: Int -> [Int] -> Bool
distinct_subset_sums n xs = distinct sums
  where
    subs = subseqs_len n xs
    sums = map sum subs

semi_special :: [Int] -> Bool
semi_special ns = and distincts
  where
    l = length ns
    m = l `div` 2
    lo = take (m + 1) ns
    hi = take m (reverse ns)
    distincts =
      [ sum xs /= sum ys |
        (xs, zs) <- partitionPairs ns,
        not (null xs),
        ys <- subseqs_len (length xs) zs ]

is_special :: [Int] -> Bool
is_special ns =
    sorted 0 ns
    && (sum lo > sum hi || l == 0)
    && and distincts
  where
    sorted m [] = True
    sorted m (n : ns) = m < n && sorted n ns
    l = length ns
    m = l `div` 2
    lo = take (m + 1) ns
    hi = take m (reverse ns)
    distincts =
      [ sum xs /= sum ys |
        (xs, zs) <- partitionPairs ns,
        not (null xs),
        ys <- subseqs_len (length xs) zs ]

special_sum_sets :: [[Int]]
special_sum_sets =
  filter (distinct_subset_sums 3) special_sum_candidates

main :: IO String
main = return $ concatMap show $ head $ special_sum_sets
-- 20313839404245

-- FIXME: clean up this code!