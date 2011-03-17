module Euler307 where
import EulerLib (showFloat)
import Data.Ratio

{-

Problem 307
Chip Defects

24 October 2010

k defects are randomly distributed amongst n integrated-circuit chips
produced by a factory (any number of defects may be found on a chip
and each defect is independent of the other defects).

Let p(k,n) represent the probability that there is a chip with at
least 3 defects. For instance p(3,7) ≈ 0.0204081633.

Find p(20 000, 1 000 000) and give your answer rounded to 10 decimal
places in the form 0.abcdefghij

-}

{-

Total number of distributions of defects:
n^k

Number of distributions where no chip has more than 1 defect:
n*(n-1)*(n-2) ...   [k terms]
n! / (n-k)!

Number of distributions where no chip has more than 2 defects:

First, select the number of pairs to make:
0 <= 2p <= k

For a given p, we then choose...
1) Which 2p of k defects will be paired:
    choose(k,2p)
    k! / (2p)!(k-2p)!
2) How those 2p values will be paired up:
    1*3*5*...*(2p-1)
    (2p)! / (2^p)(p!)
3) How to assign the (k-p) defect combos to chips, without replacement
    n*(n-1)*(n-2)*... [k-p terms]
    n! / (n-(k-p))!

We take the product of all 3 numbers.
    (k! / (2p)!(k-2p)!) * ((2p)! / (2^p)(p!)) * (n! / (n-(k-p))!)
We can cancel the factorial of 2p.
    k!/(k-2p)! * 1/(2^p)(p!) * n!/(n-(k-p))!

                     k!  n!
t(k,n,p) = ---------------------------
           (k-2p)! (2^p) (p!) (n-k+p)!

                           k!  n!
t(k,n,p+1) = -------------------------------------
             (k-2p-2)! (2^(p+1)) (p+1)! (n-k+p+1)!


t(k,n,p+1)    (k-2p)(k-2p-1)
---------- = ---------------
 t(k,n,p)    2(p+1)(n-k+p+1)

             n!
t(k,n,0) = ------
           (n-k)!

Then sum these products over all choices for p.

-}

-- n! / (n-k)!
factorial_ratio n k = f n k 1
  where
    -- f n k x = x * n! / (n-k)!
    f n 0 x = x
    f n k x = f (n-1) (k-1) $! (n*x)

prob307 k n = 1 - sum_ts % (n^k)
  where
    pmax = k`div`2
    t0 = factorial_ratio n k
    sum_ts = add t0 t0 0
    add total t p
      | p == pmax = total
      | otherwise = add (total+t') t' (p+1)
      where
        t' = (t*(k-2*p)*(k-2*p-1))`div`(2*(p+1)*(n-k+p+1))

main :: IO String
main = return $ showFloat 10 $ prob307 20000 1000000

answer :: String
answer = "0.7311720251"
