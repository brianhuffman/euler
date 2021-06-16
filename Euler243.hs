module Euler243 where
import Data.Ratio

import Primes
import Permutation
import EulerLib

{-
Problem 243
Resilience

02 May 2009

A positive fraction whose numerator is less than its denominator is
called a proper fraction.  For any denominator, d, there will be d−1
proper fractions; for example, with d = 12:

1/12, 2/12, 3/12, 4/12, 5/12, 6/12, 7/12, 8/12, 9/12, 10/12, 11/12

We shall call a fraction that cannot be cancelled down a resilient
fraction.  Furthermore we shall define the resilience of a
denominator, R(d), to be the ratio of its proper fractions that are
resilient; for example, R(12) = 4/11.  In fact, d = 12 is the smallest
denominator having a resilience R(d) < 4/10.

Find the smallest denominator d, having a resilience R(d) <
15499/94744.

-}

{-

Resilience is nearly multiplicative.

totient(d)/d *is* multiplicative.

totient(p)/p = (p-1)/p
totient(p^k)/(p^k) = (p-1)/p

-}


resilience :: Integer -> Rational
resilience d = totient d % (d-1)

cutoff :: Rational
cutoff = 15499/94744

main :: IO String
main = return $ show $ 0

answer :: String
answer = "892371480"


-- 6469693230 WRONG!
-- 1115464350 WRONG!
-- 892371480 RIGHT!

-- 892371480 = 2^3 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23
