module Euler186 where
import Lagged
import List
import Control.Monad.ST
import Data.Array.ST

{-
Problem 186
Connectedness of a network.

15 March 2008

Here are the records from a busy telephone system with one million users:
RecNr   Caller  Called
1       200007  100053
2       600183  500439
3       600863  701497
...     ...     ...

The telephone number of the caller and the called number in record n are
Caller(n) = S(2n-1) and Called(n) = S(2n) where S(1), S(2), S(3), ... come
from the "Lagged Fibonacci Generator":

For 1 <= k <= 55, S(k) = [100003 - 200003k + 300007k^3] (modulo 1000000)
For 56 <= k, S(k) = [S(k-24) + S(k-55)] (modulo 1000000)

If Caller(n) = Called(n) then the user is assumed to have misdialled and the
call fails; otherwise the call is successful.

From the start of the records, we say that any pair of users X and Y are
friends if X calls Y or vice-versa. Similarly, X is a friend of a friend
of Z if X is a friend of Y and Y is a friend of Z; and so on for longer
chains.

The Prime Minister's phone number is 524287. After how many successful calls,
not counting misdials, will 99% of the users (including the PM) be a friend,
or a friend of a friend etc., of the Prime Minister?
-}

-- Disjoint sets, with size counting
-- If d!n >=0, then it points to its parent
-- If d!n < 0, then it is a root, with size -(d!n)

type DisjSet s = STUArray s Int Int

-- discrete partition, every node is a root of size 1.
newDisjSet :: Int -> ST s (DisjSet s)
newDisjSet m = newArray (0, m-1) (-1)

-- lookup the root and size of a given node
rootDisjSet :: DisjSet s -> Int -> ST s (Int, Int)
rootDisjSet a n = do
    (ns,r,s) <- root [] n
    sequence_ [ writeArray a n r | n <- ns ]
    return (r,s)
  where
    root ns n = do
      m <- readArray a n
      if m < 0
        then return (ns,n,-m)
        else root (n:ns) m

unionDisjSet :: DisjSet s -> Int -> Int -> ST s ()
unionDisjSet a m n = do
  (m', sm) <- rootDisjSet a m
  (n', sn) <- rootDisjSet a n
  if m' == n' then return () else do
    writeArray a m' n'
    writeArray a n' (-(sm + sn))

minister :: Int
minister = 524287

{-
prob186z calls = do
  a <- newDisjSet (10^6)
  sequence_ [ unionDisjSet a m n | (m,n) <- calls ]
  (r,s) <- rootDisjSet a minister
  return s

prob186y k = runST (prob186z (take k phone_calls))
-}

prob186 z v = do
  a <- newDisjSet (10^6)
  f 1 a phone_calls
  where
    f n a ((i,j):cs) = do
      unionDisjSet a i j
      (r,s) <- rootDisjSet a v
      if s >= z then return n
        else (f $! (n+1)) a cs

phone_calls :: [(Int, Int)]
phone_calls = pairs lagged_fibonacci
  where
    pairs (a:b:cs)
      | a == b = pairs cs
      | otherwise = (a, b) : pairs cs

main :: IO String
main = return $ show $ runST (prob186 990000 524287)
-- 2325629
