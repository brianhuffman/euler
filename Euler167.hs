module Euler167 where
import EulerLib
import qualified SortedList as S
import Data.List

{-
Problem 167
Investigating Ulam sequences

09 November 2007

For two positive integers a and b, the Ulam sequence U(a,b) is defined by
U(a,b)[1] = a, U(a,b)[2] = b and for k > 2, U(a,b)[k] is the smallest integer
greater than U(a,b)[k-1] which can be written in exactly one way as the sum of
two distinct previous members of U(a,b).

For example, the sequence U(1,2) begins with
1, 2, 3 = 1 + 2, 4 = 1 + 3, 6 = 2 + 4, 8 = 2 + 6, 11 = 3 + 8;
5 does not belong to it because 5 = 1 + 4 = 2 + 3 has two representations as
the sum of two previous members, likewise 7 = 1 + 6 = 3 + 4.

Find U(2,2n+1)[k] for 2 <= n <= 10, where k = 10^11.
-}

ulam a b = a : b : f [a,b] [b,a] (b+1)
  where
    f xs ys n = case g xs ys n of
      [(),()] -> n : f (xs ++ [n]) (n : ys) (n+1)
      _ -> f xs ys (n+1)
    g (x:xs) (y:ys) n = case compare (x+y) n of
      LT -> g xs (y:ys) n
      GT -> g (x:xs) ys n
      EQ -> () : g xs ys n
    g _ _ _ = []

deltas xs = zipWith subtract xs (tail xs)

{-
U(2,5) deltas:
[3,2,2,2,1,1] ++ cycle
[2,4,4,4,2,6,2,4,2,2,4,2,4,6,6,2,2,8,4,2,2,2,6,4,8,2,10,12,2,2,2,2]
(length 32, sum 126)

U(2,7) deltas:
[5,2,2,2,2,1,1] ++ cycle
[2,2,4,4,4,4,2,6,2,6,8,8,2,2,2,10,4,12,2,14,16,2,2,2,2,2]
(length 26, sum 126)

U(2,9) deltas:
[7,2,2,2,2,2,1,1] ++ cycle
[2,2,2,4,4,4,4,4,2,6,2,6,2,4,2,2,4,2,2,4,2,4,6,4,2,4,2,8,2,4,2,4,2,2,2,4,2,4,2,4,4,2,4,2,4,2,6,6,6,8,2,2,8,2,2,2,4,10,4,6,2,2,2,2,6,2,2,4,4,8,4,2,6,2,2,2,6,8,4,8,2,2,2,6,2,2,2,4,4,2,2,4,4,2,6,4,2,6,8,2,4,2,2,10,6,4,2,2,2,2,8,2,4,4,10,6,2,12,2,2,4,2,2,2,2,2,4,6,4,4,6,2,2,6,2,8,4,2,2,4,2,2,2,6,4,2,4,4,2,2,6,6,2,4,8,2,2,4,2,10,4,2,4,2,2,2,2,6,6,4,4,2,2,8,2,6,4,2,2,2,4,2,2,6,4,6,4,2,2,6,2,2,6,4,2,2,4,8,2,4,6,2,2,2,4,2,8,4,6,10,2,8,2,2,2,2,4,2,2,2,4,4,6,4,6,2,8,2,8,10,10,2,2,2,2,12,4,4,2,2,2,2,2,6,2,4,4,4,2,2,4,2,6,2,4,6,8,6,2,2,10,2,2,4,12,4,2,14,2,4,2,2,2,2,2,2,4,2,4,4,4,6,6,2,6,2,2,8,8,4,2,2,2,10,2,4,4,2,2,2,2,4,2,6,4,4,2,4,2,2,6,2,4,2,4,8,6,6,2,2,2,8,2,2,4,4,2,2,2,4,6,2,4,4,2,8,6,2,4,2,2,2,8,6,4,10,2,2,6,2,2,2,2,4,8,4,4,2,10,2,6,12,8,2,2,2,2,2,10,4,4,12,2,6,2,2,2,2,2,4,2,2,4,4,4,2,4,6,2,6,6,2,2,4,2,2,8,4,2,4,10,2,4,2,12,6,14,2,2,16,4,2,2,2,2,2,2,2,6,4,4,4,8,2,6,2,10,8,12,2,2,2,14,4,16,2,18,20,2,2,2,2,2,2]
(length 444, sum 1778)

U(2,11) deltas:
[9,2,2,2,2,2,2,1,1] ++ cycle ...
(length 1628, sum 6510)

U(2,15) deltas:
[13,2,2,2,2,2,2,2,2,1,1] ++ cycle
[2,2,2,2,2,2,4,4,4,4,4,4,4,4,2,6,2,6,2,6,2,6,8,8,8,8,2,2,2,10,2,2,2,10,4,12,4,12,2,14,2,14,16,16,2,2,2,2,2,2,2,18,4,4,4,20,2,6,2,22,8,24,2,2,2,26,4,28,2,30,32,2,2,2,2,2,2,2,2,2]
(length 80, sum 510)

U(2,11) deltas:
[9,2,2,2,2,2,2,1,1] ++ cycle ...
(length 1628, sum 6510)

U(2,5) deltas: (prefix 6, length 32, sum 126)
U(2,7) deltas: (prefix 7, length 26, sum 126)
U(2,9) deltas: (prefix 8, length 444, sum 1778)
U(2,11) deltas: (prefix 9, length 1628, sum 6510)
U(2,13) deltas: (prefix 10, length 5906, sum 23622)
U(2,15) deltas: (prefix 11, length 80, sum 510)
U(2,17) deltas: (prefix 12, length 126960, sum 507842)
U(2,19) deltas: (prefix 13, length 380882, sum 1523526)
U(2,21) deltas: (prefix 14, length 2097152, sum 8388606)

Eventually differences are periodic!
-}

-- fast_ulam n = ulam 2 (2*n+1)
fast_ulam :: Integer -> [Integer]
fast_ulam n = S.union [2,2*d] (f b us)
  where
    b = 2*n+1
    d = 2*n+2
    us = replicate (fromIntegral d) True ++ vs
    vs = zipWith (/=) us (True : vs)
    f n (True:ts) = n : (f $! (n+2)) ts
    f n (False:ts) = (f $! (n+2)) ts

prob167 = sum [ ulam n l s (10^11) | (n,l,s) <- zip3 ns lengths sums ]
  where
    ns = [2 .. 10]
    lengths = [32,26,444,1628,5906,80,126960,380882,2097152]
    sums = [126,126,1778,6510,23622,510,507842,1523526,8388606]
    ulam n l s i =
      let d = n+4
          j = i - d
          (q, r) = divMod j l
      in (fast_ulam n !! fromIntegral (r+d-1)) + s * q

main :: IO String
main = return $ show $ prob167
-- 3916160068885
