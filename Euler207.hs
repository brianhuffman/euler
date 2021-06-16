module Euler207 where
import Data.Ratio

{-
For some positive integers k, there exists an integer partition of the form
4^t = 2^t + k, where 4^t, 2^t, and k are all positive integers and t is a
real number.

The first two such partitions are 4^1 = 2^1 + 2 and
4^1.5849625... = 2^1.5849625... + 6.

Partitions where t is also an integer are called perfect.
For any m >= 1 let P(m) be the proportion of such partitions that are
perfect with k <= m.
Thus P(6) = 1/2.

In the following table are listed some values of P(m)

   P(5) = 1/1
   P(10) = 1/2
   P(15) = 2/3
   P(20) = 1/2
   P(25) = 1/2
   P(30) = 2/5
   ...
   P(180) = 1/4
   P(185) = 3/13

Find the smallest m for which P(m) < 1/12345
-}

part n = n^2 - n
partitions = [ part n | n <- [2 ..] ]
perfect_partitions = [ part (2^t) | t <- [1 ..] ]

{-
ratios (x:xs) (y:ys) (m, n) =
  case compare x y of
    EQ -> (x, (m+1)%(n+1)) : ratios xs ys (m+1, n+1)
    LT -> (x, (m+1)%n) : ratios xs (y:ys) (m+1, n)
    GT -> (y, m%(n+1)) : ratios (x:xs) ys (m, n+1)
ratios _ _ _ = []
-}

-- P(m)
prob207_P m = p % q
  where
    p = length (takeWhile (<= m) perfect_partitions)
    q = length (takeWhile (<= m) partitions)

{-
Every time we get to a perfect partition, the ratio goes up.
To find a minimum, we should look just before each perfect partition.
-}

-- smallest m for which P(m) < x
prob207 x = part n
  where
    r (n, t) = t % (n-1)
    nts = [ (2^t - 1, t-1) | t <- [2 ..] ]
    (n0, t) = head $ dropWhile ((>= x) . r) nts
    n = floor (fromIntegral t / x + 1) + 1
{-
Least n. t/(n-1) < x
Least n. t < x * (n-1)
Least n. t/x < n-1
Least n. t/x + 1 < n
-}

main :: IO String
main = return $ show $ prob207 (1/12345)
