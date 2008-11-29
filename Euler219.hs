module Euler219 where

{-
Problem 219
29 November 2008

Let A and B be bit strings (sequences of 0's and 1's). If A is equal
to the leftmost length(A) bits of B, then A is said to be a prefix of
B.  For example, 00110 is a prefix of 001101001, but not of 00111 or
100110.

A prefix-free code of size n is a collection of n distinct bit strings
such that no string is a prefix of any other. For example, this is a
prefix-free code of size 6:

0000, 0001, 001, 01, 10, 11

Now suppose that it costs one penny to transmit a '0' bit, but four
pence to transmit a '1'.  Then the total cost of the prefix-free code
shown above is 35 pence, which happens to be the cheapest possible for
the skewed pricing scheme in question.  In short, we write Cost(6) =
35.

What is Cost(10^9) ?
-}

{-
Cost(1) = 0, cheapest code is [""], with costs [0].
Cost(2) = 5, cheapest code is ["0","1"], with costs [1,4].
Cost(3) = 11, cheapest code is ["00","1","01"], with costs [2,4,5].
Cost(4) = 18, cheapest code has costs [3,4,5,6].
Cost(5) = 26, cheapest code has costs [4,4,5,6,7].
Cost(7) = 44, cheapest code has costs [5,5,5,6,7,8,8].

 k     x     y     z     n     t
--------------------------------
 5     1     1     1     1     0
 6     1     1     1     2     5
 7     1     1     2     3    11
 8     1     2     3     4    18
 9     2     3     4     5    26
10     3     4     5     7    44
11     4     5     7    10    74

k = marginal cost of increasing size by 1.
k = cost of least expensive code + 5.
x = number of codes with cost (k-5).
y = number of codes with cost (k-5) or (k-4).
z = number of codes with cost (k-5), (k-4), or (k-3).
n = total number of all codes.
t = total cost of all codes.

Recurrence:
(k, x, y, z, n, t) -> (k+1, y, z, n, n+x, t+n*k)

-}

cost :: Integer -> Integer
cost m = f 5 1 1 1 1 0
  where
    f k x y z n t
      | n + x > m = t + k*(m - n)
      | otherwise = f (k+1) y z n (n+x) (t + k*x)

main :: IO String
main = return $ show $ cost (10^9)
-- 64564225042
