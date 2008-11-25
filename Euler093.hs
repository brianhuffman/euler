module Euler093 where
import Permutation
import qualified SortedList as S
import Data.List

{-
Problem 93
Using four distinct digits and the rules of arithmetic,
  find the longest sequence of target numbers.

15 April 2005

By using each of the digits from the set, {1, 2, 3, 4}, exactly once, and
making use of the four arithmetic operations (+, , *, /) and brackets/
parentheses, it is possible to form different positive integer targets.

For example,

8 = (4 * (1 + 3)) / 2
14 = 4 * (3 + 1 / 2)
19 = 4 * (2 + 3) 1
36 = 3 * 4 * (2 + 1)

Note that concatenations of the digits, like 12 + 34, are not allowed.

Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one different
target numbers of which 36 is the maximum, and each of the numbers 1 to 28
can be obtained before encountering the first non-expressible number.

Find the set of four distinct digits, a < b < c < d, for which the longest
set of consecutive positive integers, 1 to n, can be obtained, giving your
answer as a string: abcd.
-}

{-
Left side of (+) should not be (+):
(a + b) + c = a + (b + c)

Neither side of (+) or (-) should be (-):
(a - b) - c = a - (b + c)
a - (b - c) = (a + c) - b
(a - b) - (c - d) = (a + d) - (b + c)
(a - b) + c = (a + c) - b
a + (b - c) = (a + b) - c
(a - b) + (c - d) = (a + c) - (b + d)

-}

raw_results :: [Rational] -> [Rational]
raw_results [] = []
raw_results [x] = [x]
raw_results xs =
  [ f y z |
    (ys, zs) <- partitionPairs xs,
    not (null ys), not (null zs),
    y <- raw_results ys,
    z <- raw_results zs,
    f <- if z == 0 then [(+),(*)] else [(+),(-),(*),(/)]
  ]

results :: [Rational] -> [Rational]
results [] = []
results [x] = [x]
results xs = S.nub $ sort $
  [ f y z |
    (ys, zs) <- partitionPairs xs,
    not (null ys), not (null zs),
    y <- results ys,
    z <- results zs,
    f <- ops y z
  ]
  where
    ops y z
      | z == 0 = [(+),(*)]
      | z < y = [(-),(/)]
      | otherwise = [(+),(-),(*),(/)]

prob93 n = maximum
  [ (m, concatMap show ns) |
    ns <- subseqs_len n [1 .. 9],
    let ds = map toRational ns,
    let m = head (S.deleteFirsts [1 ..] (results ds))
  ]
-- prob93 4 = (52%1, "1258")
-- prob93 5 = (193%1, "45678")

main :: IO String
main = return $ snd (prob93 4)
-- "1258"

