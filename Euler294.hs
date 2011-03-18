module Euler294 where
import EulerLib (funArray)
import Data.Array

{-

Problem 294
Sum of digits - experience #23
29 May 2010

For a positive integer k, define d(k) as the sum of the digits of k in
its usual decimal representation. Thus d(42) = 4+2 = 6.

For a positive integer n, define S(n) as the number of positive
integers k < 10^n with the following properties :

    * k is divisible by 23 and
    * d(k) = 23. 

You are given that S(9) = 263626 and S(42) = 6377168878570056.

Find S(11^12) and give your answer mod 10^9.

-}


{-

Powers of 10 (mod 23):
rs = [1,10,8,11,18,19,6,14,2,20,16,22,13,15,12,5,4,17,9,21,3,7]
length rs = 22

(10^n`mod`23) == rs!!(n`mod`22)

We have 23 identical units, each of which is assigned to one of 11^12
positions. An item in position n receives a score of 10^n`mod`23. The
total score must == 0 (mod 23). No position is allowed to hold more
than 9 units.

-}

-- integers from [0..p-1]
type Residue = Int
type Modulus = Int

choose :: Integer -> Integer -> Integer
choose n 0 = 1
choose n k = choose n (k-1) * (n-k+1) `div` k

-- combinations with replacement
choose_rep :: Integer -> Integer -> Integer
choose_rep n k = choose (n+k-1) k

-- combinations with limited replacement:
-- each slot can hold at most m items
choose_limrep :: Integer -> Integer -> Integer -> Integer
choose_limrep m n k
  | k < 1*(m+1) = choose_rep n k
  | k < 2*(m+1) = choose_rep n k - singles
  | k < 3*(m+1) = choose_rep n k - singles + doubles
  | otherwise = error "choose_limrep: limit too small"
  where
    singles = n * sum [ choose_rep (n-1) (k-d) | d <- [m+1..k] ]
    doubles = choose n 2 * sum
      [ choose_rep (n-2) (k-d1-d2) | d1 <- [m+1..k], d2 <- [m+1..k-d1] ]

choose_limrep_slow m n 0 = 1
choose_limrep_slow m 0 k = 0
choose_limrep_slow m n k = sum
  [ choose_limrep_slow m (n-1) (k-d) | d <- [0..min m k] ]

choose_limrep_list m 0 0 = [[]]
choose_limrep_list m 0 k = []
choose_limrep_list m n k =
  [ d:ds | d <- [0..min m k], ds <- choose_limrep_list m (n-1) (k-d) ]

---------------------------------------

powers :: Modulus -> [Residue]
powers p = take (p-1) (iterate (\x -> (x*10)`mod`p) 1)

type Table = Array Residue Integer

-- for each residue r, how many ways to allocate 1 unit
-- in 'size' slots, yielding score r
table1 :: Modulus -> Integer -> Table
table1 p size = accumArray (+) 0 (0, p-1) (upds1 ++ upds2)
  where
    (q, r) = size `divMod` toInteger (p-1)
    upds1 = [ (n, q) | n <- [1..p-1] ]
    upds2 = take (fromIntegral r) [ (n, 1) | n <- powers p ]

prob294 :: Modulus -> Int -> Integer -> Integer
prob294 p n size = a!(n,p-1)!0
  where
    t1 = table1 p size
    -- f(n,m)!r = ways to allocate n units, each using only
    -- positions with scores up to m, with total score r.
    a = funArray ((0,0),(n,p-1)) f
    f(0,m) = accumArray (+) 0 (0,p-1) [(0,1)]
    f(n,0) = accumArray (+) 0 (0,p-1) []
    f(n,m) = accumArray (+) 0 (0,p-1)
      [ (r', x*y) |
        d <- [0..n],
        let x = choose_limrep 9 (t1!m) (toInteger d),
        (r,y) <- assocs (a!(n-d,m-1)),
        let r' = (d*m+r)`mod`p ]

main :: IO String
main = return $ show $ prob294 23 23 (11^12) `mod` (10^9)

answer :: String
answer = "789184709"
