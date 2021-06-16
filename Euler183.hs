module Euler183 where
import EulerLib
import Primes
import Data.Ratio

{-
Problem 183

22 February 2008

Let N be a positive integer and let N be split into k equal parts, r = N/k,
so that N = r + r + ... + r.
Let P be the product of these parts, P = r * r * ... * r = r^k.

For example, if 11 is split into five equal parts, 11 = 2.2 + 2.2 + 2.2 +
2.2 + 2.2, then P = 2.2^5 = 51.53632.

Let M(N) = Pmax for a given value of N.

It turns out that the maximum for N = 11 is found by splitting eleven into
four equal parts which leads to Pmax = (11/4)4; that is, M(11) = 14641/256 =
57.19140625, which is a terminating decimal.

However, for N = 8 the maximum is achieved by splitting it into three equal
parts, so M(8) = 512/27, which is a non-terminating decimal.

Let D(N) = N if M(N) is a non-terminating decimal and D(N) = -N if M(N) is
a terminating decimal.

For example, ΣD(N) for 5 <_ N <= 100 is 2438.

Find ΣD(N) for 5 <= N <= 10000.
-}

{-
The value of k which maximizes (n/k)^k also maximizes log((n/k)^k).

log ((n/k)^k)
= k * log (n/k)
= k * (log n - log k)
= k * log n - k * log k

for k', ((k'+1) * log n - (k'+1) * log (k'+1)) - (k' * log n - k' * log k') < 0
for kmax, log n < nlogn_diff(kmax)
for k<kmax, log n > nlogn_diff(k)
-}
prob183M :: [(Integer,Integer)]
prob183M = f 1 1
  where
    ln k = log (fromIntegral k)
    nlogn k = fromIntegral k * ln k
    logdiff k = nlogn (k+1) - nlogn k
    f n k
      | ln n < logdiff k = (n,k) : f (n+1) k
      | otherwise = f n (k+1)

terminating_denominator n =
  fst (fst (n `divN` 2) `divN` 5) == 1

prob183D (n,k)
  | terminating_denominator (denominator (n % k)) = - n
  | otherwise = n

prob183 m = sum (map prob183D (drop 4 $ take m prob183M))

main :: IO String
main = return $ show $ prob183 10000
-- 48861552
