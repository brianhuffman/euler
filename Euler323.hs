module Euler323 where
import EulerLib

{-

Problem 323
Bitwise-OR operations on random integers
06 February 2011

Let y[0], y[1], y[2],... be a sequence of random unsigned 32 bit
integers (i.e. 0 ≤ y[i] < 2^32, every value equally likely).

For the sequence x[i] the following recursion is given:

  * x[0] = 0 and
  * x[i] = x[i-1] | y[i-1], for i > 0. ( | is the bitwise-OR operator)

It can be seen that eventually there will be an index N such that x[i]
= 2^32-1 (a bit-pattern of all ones) for all i ≥ N.

Find the expected value of N. Give your answer rounded to 10 digits
after the decimal point.

-}

{-

Starting with n zero bits, after one more step, the probability of
having k zeroes remain is (choose n k / 2^n).

e(0) = 0
e(1) = 1 + (e(0) + e(1))/2
e(2) = 1 + (e(0) + 2e(1) + e(2))/4
...

e(n) = 1 + (SUM i:[0..n]. choose n i * e(i)) / 2^n

e(n) = 1 + (SUM i:[0..n-1]. choose n i * e(i)) / 2^n + e(n) / 2^n

((2^n-1)/2^n) e(n) = 1 + (SUM i:[0..n-1]. choose n i * e(i)) / 2^n

(2^n-1) e(n) = 2^n + (SUM i:[0..n-1]. choose n i * e(i))
e(n) = (2^n + (SUM i:[0..n-1]. choose n i * e(i))) / (2^n-1)

-}

prob323 :: [Rational]
prob323 = es
  where
    es = 0 : f [1,1] 2
    next row = zipWith (+) ([0] ++ row) (row ++ [0])
    f row total = e : f (next row) (2 * total)
      where
        e = (total + sum (zipWith (*) (init row) es)) / (total - 1)

main :: IO String
main = return $ showFloat 10 $ prob323 !! 32

answer :: String
answer = "6.3551758451"
