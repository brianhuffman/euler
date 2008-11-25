module Euler156 where

{-
Problem 156
Counting Digits

25 May 2007

Starting from zero the natural numbers are written down in base 10 like this:
0 1 2 3 4 5 6 7 8 9 10 11 12....

Consider the digit d=1. After we write down each number n, we will update the
number of ones that have occurred and call this number f(n,1). The first values
for f(n,1), then, are as follows:

n	f(n,1)
0	0
1	1
2	1
3	1
4	1
5	1
6	1
7	1
8	1
9	1
10	2
11	4
12	5

Note that f(n,1) never equals 3.
So the first two solutions of the equation f(n,1)=n are n=0 and n=1. The next
solution is n=199981.

In the same manner the function f(n,d) gives the total number of digits d that
have been written down after the number n has been written. In fact, for every
digit d > 0, 0 is the first solution of the equation f(n,d)=n.

Let s(d) be the sum of all the solutions for which f(n,d)=n.
You are given that s(1)=22786974071.

Find SUM s(d) for 1 <= d <= 9.

Note: if, for some n, f(n,d)=n for more than one value of d this value of n is
counted again for every value of d for which f(n,d)=n.
-}

------------------------------------------------------------------------------
-- 156. 

-- # of occurrences of digit d in number n
count_digit 0 d = 0
count_digit n d = case n `divMod` 10 of
      (q, r) -> count_digit q d + (if d == r then 1 else 0)

-- # of occurrences of digit d in all numbers less than n
count_digit_less 0 d = 0
count_digit_less n d = case n `divMod` 10 of
  (q, r) ->
    (if r <= d then q else q+1) +  -- in the ones place
    10 * count_digit_less q d +
    r * count_digit q d

{-
count_digit_less (10*q) d = q + 10 * count_digit_less q d
count_digit_less (10^n) d = n * 10^(n-1)
-}

-- # of occurrences of digit d in all numbers less than or equal to n
count_digit_upto 0 d = 0
count_digit_upto n d = case n `divMod` 10 of
  (q, r) ->
    (if r < d then q else q+1) +  -- in the ones place
    10 * count_digit_upto q d -
    (9-r) * count_digit q d

{-
f(10n, d) = n + 10 f(n,d)
f(100n, d) = 10n + 10 (n + 10 f(n,d))
           = 10n + 10n + 100 f(n,d)
           = 20n + 100 f(n, d)
f(1000n, d) = 300n + 1000 f(n,d)
f(n*10^k, d) = n*k * 10^(k-1) + 10^k * f(n,d)

f(n * 10^10, d) = n * 10 * 10^9 + 10^10 * f(n, d)
f(n * 10^10, d) = (n + f(n, d)) * 10^10

For n > d * 10^10, f(n,d) is strictly greater than n.
-}

solutions d = fixes 0
  where
    f n = count_digit_upto n d
    nmax = d * 10^10
    fixes n
      | n > nmax = []
      | otherwise = case compare (f n) n of
          GT -> fixes (f n)
          EQ -> n : fixes (n+1)
          LT -> find n 1
    -- precondition: f n < n
    -- postcondition: f (m-1) < (m-1) and f m >= m
    find n s
      | n > nmax  = []
      | f m < n   = find m (s * 2)
      | s > 1     = find n (s `div` 2)
      | otherwise = fixes m
      where m = n + s

main :: IO String
main = return $ show $ sum [ sum (solutions d) | d <- [1 .. 9] ]
-- 21295121502550
