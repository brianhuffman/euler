module Euler310 where
import EulerLib
import Data.Array
import Data.Bits
import qualified Data.IntSet as S

{-

Problem 310
Nim Square

13 November 2010

Alice and Bob play the game Nim Square. Nim Square is just like
ordinary three-heap normal play Nim, but the players may only remove a
square number of stones from a heap. The number of stones in the three
heaps is represented by the ordered triple (a,b,c). If 0≤a≤b≤c≤29 then
the number of losing positions for the next player is 1160.

Find the number of losing positions for the next player if
0≤a≤b≤c≤100 000.

-}

{-

(100,000)^3 = 10^15
We don't have nearly enough memory to store the whole table!
We *can* find the nim-scores for each pile separately.

-}

mex :: [Int] -> Int
mex xs = f 0 (S.toAscList (S.fromList xs))
  where
    f i [] = i
    f i (n : ns) = if i < n then i else f (i+1) ns

-- nim-scores for the one-pile game
table1 :: Int -> Array Int Int
table1 m = a
  where
    a = funArray (0,m) f
    f n = mex [ a!(n-k) | k <- takeWhile (<=n) (map (^2) [1..]) ]

-- Array that returns the set of indices from table1 with a given score.
table1_sets :: Int -> Int -> Array Int S.IntSet
table1_sets s m = t3
  where
    t1 = table1 m
    rev_assocs = [ (t1!n, n) | n <- [m,m-1..0] ]
    t2 = accumArray (flip (:)) [] (0,s) rev_assocs
    t3 = fmap S.fromDistinctAscList t2

prob310_slow m = sum
  [ S.size cs |
    a <- [0 .. m],
    b <- [a .. m],
    let s = xor (t1!a) (t1!b),
    -- positions >= b with score s
    s <= smax,
    let cs = snd (S.split (b-1) (t3!s)) ]
  where
    t1 = table1 m
    smax = maximum (elems t1)
    rev_assocs = [ (t1!n, n) | n <- [m,m-1..0] ]
    t2 = accumArray (flip (:)) [] (0,smax) rev_assocs
    t3 = fmap S.fromDistinctAscList t2

-- 100: 25582
-- 200: 179663
-- 500: 2018811
-- 1000: 12430812
-- 10000: too slow!

prob310_slow' m = sum
  [ S.size cs |
    a <- [0 .. m],
    b <- [0 .. m],
    let s = xor (t1!a) (t1!b),
    -- positions >= b with score s
    s <= smax,
    let cs = t3!s ]
  where
    t1 = table1 m
    smax = maximum (elems t1)
    rev_assocs = [ (t1!n, n) | n <- [m,m-1..0] ]
    t2 = accumArray (flip (:)) [] (0,smax) rev_assocs
    t3 = fmap S.fromDistinctAscList t2
-- 50: 26611
-- 100: 147087
-- 200: 1054988
-- 500: 12004506

{-

Faster strategy: loop over scores instead of positions.

w: Positions with a < b < c: counted 6 times.
x: Positions with a = b < c: counted 3 times.
y: Positions with a < b = c: counted 3 times.
z: Positions with a = b = c: counted 1 time.

prob310' calculates 6w + 3x + 3y + z --- we want w+x+y+z.

If a = b, then nim-score(a,b,c) = nim-score(c).
x+y = m*count(0)
z = count(0)

We must add an extra (3m+5)*count(0) = 3(x+y)+5z, then divide by 6.

-}

prob310' :: Int -> Integer
prob310' m = sum
  [ toInteger k1 * toInteger k2 * toInteger k3 |
    s1 <- [0 .. smax],
    let k1 = count!s1,
    s2 <- [0 .. smax],
    let k2 = count!s2,
    let s3 = xor s1 s2, s3 <= smax,
    let k3 = count!s3 ]
  where
    t1 = table1 m
    smax = maximum (elems t1)
    count :: Array Int Int
    count = accumArray (+) 0 (0,smax) [ (s,1) | s <- elems t1 ]

prob310 :: Int -> Integer
prob310 m = (total + toInteger (3*m+5) * toInteger k0) `div` 6
  where
    t1 = table1 m
    smax = maximum (elems t1)
    count :: Array Int Int
    count = accumArray (+) 0 (0,smax) [ (s,1) | s <- elems t1 ]
    k0 = count!0
    total = sum
      [ toInteger k1 * toInteger k2 * toInteger k3 |
        s1 <- [0 .. smax],
        let k1 = count!s1,
        s2 <- [0 .. smax],
        let k2 = count!s2,
        let s3 = xor s1 s2, s3 <= smax,
        let k3 = count!s3 ]

main :: IO String
main = return $ show $ prob310 100000

answer :: String
answer = "2586528661783"
