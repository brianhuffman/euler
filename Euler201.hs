module Euler201 where
import EulerLib

{-
For any set A of numbers, let sum(A) be the sum of the elements of A.
Consider the set B = {1,3,6,8,10,11}.
There are 20 subsets of B containing three elements, and their sums are:

sum({1,3,6}) = 10,
sum({1,3,8}) = 12,
sum({1,3,10}) = 14,
sum({1,3,11}) = 15,
sum({1,6,8}) = 15,
sum({1,6,10}) = 17,
sum({1,6,11}) = 18,
sum({1,8,10}) = 19,
sum({1,8,11}) = 20,
sum({1,10,11}) = 22,
sum({3,6,8}) = 17,
sum({3,6,10}) = 19,
sum({3,6,11}) = 20,
sum({3,8,10}) = 21,
sum({3,8,11}) = 22,
sum({3,10,11}) = 24,
sum({6,8,10}) = 24,
sum({6,8,11}) = 25,
sum({6,10,11}) = 27,
sum({8,10,11}) = 29.

Some of these sums occur more than once, others are unique.
For a set A, let U(A,k) be the set of unique sums of k-element subsets of A, in our example we find U(B,3) = {10,12,14,18,21,25,27,29} and sum(U(B,3)) = 156.

Now consider the 100-element set S = {1^2, 2^2, ... , 100^2}.
S has 100891344545564193334812497256 50-element subsets.

Determine the sum of all integers which are the sum of exactly one of the 50-element subsets of S, i.e. find sum(U(S,50)).
-}

test :: [Int]
test = [1,3,6,8,10,11]

squares :: [Int]
squares = [ n^2 | n <- [1 .. 100] ]

------------------------------------------
-- Generic solution

class Multiset a where
  empty :: a
  zero :: a
  union :: a -> a -> a
  shift :: Int -> a -> a

prob201_init :: Multiset a => [a]
prob201_init = zero : repeat empty

prob201_step :: Multiset a => [a] -> Int -> [a]
prob201_step xss x =
  zero : zipWith (combine x) (tail xss) xss

combine :: Multiset a => Int -> a -> a -> a
combine n xs ys = union xs (shift n ys)

-- prob201 xs !! k !! n = # of k-element subsets of xs summing to n
prob201 :: Multiset a => [Int] -> [a]
prob201 = foldl prob201_step prob201_init

------------------------------------------
-- Multisets as lists of multiplicities

newtype IntList = IntList [Int]
  deriving Show

instance Multiset IntList where
  empty = IntList []
  zero = IntList [1]
  union (IntList xs) (IntList ys) = IntList (add_zip xs ys)
  shift n (IntList xs) = IntList (replicate n 0 ++ xs)

add_zip :: [Int] -> [Int] -> [Int]
add_zip xs [] = xs
add_zip [] ys = ys
add_zip (x:xs) (y:ys) = (x+y) : add_zip xs ys

------------------------------------------
-- Multisets as run-length encoded lists

newtype RLE = RLE [(Int,Int,Bool)]
  deriving Show
-- (lo, hi, unique)

instance Multiset RLE where
  empty = RLE []
  zero = RLE [(0,0,True)]
  union (RLE xs) (RLE ys) = RLE (rle_norm (rle_union xs ys))
  shift n (RLE xs) = RLE (rle_shift n xs)

rle_shift :: Int -> [(Int,Int,Bool)] -> [(Int,Int,Bool)]
rle_shift n = map (\(x, y, u) -> (x+n, y+n, u))

rle_norm :: [(Int,Int,Bool)] -> [(Int,Int,Bool)]
rle_norm (x@(x1,x2,u):ys@(y@(y1,y2,v):zs))
  | u == v && x2+1 == y1 = rle_norm ((x1,y2,u):zs)
  | otherwise = x : rle_norm ys
rle_norm xs = xs

rle_union :: [(Int,Int,Bool)] -> [(Int,Int,Bool)] -> [(Int,Int,Bool)]
rle_union xs [] = xs
rle_union [] ys = ys
rle_union xs@(x@(x1,x2,u):xs') ys@(y@(y1,y2,v):ys')
  | x2 < y1 = x : rle_union xs' ys
  | y2 < x1 = y : rle_union xs ys'
  -- otherwise, intervals overlap
  | x1 < y1 = (x1, y1-1, u) : rle_union ((y1,x2,u):xs') ys
  | y1 < x1 = (y1, x1-1, v) : rle_union xs ((x1,y2,v):ys')
  --  otherwise, x1 == y1
  | x2 < y2 = rle_union xs ((y1, x2, v) : (x2+1, y2, v) : ys')
  | y2 < x2 = rle_union ((x1, y2, u) : (y2+1, x2, u) : xs') ys
  -- otherwise, x2 == y2
  | otherwise = (x1, x2, False) : rle_union xs' ys'

rle_uniques :: RLE -> [Int]
rle_uniques (RLE xs) =
  concat [ [x1 .. x2] | (x1, x2, u) <- xs, u ]

------------------------------------------

main :: IO String
main = return $ show $ sum $ rle_uniques $ prob201 squares !! 50

{-
Subsets with unique sums always come in pairs. Specifically, if a subset
A of S_N has a unique sum, then its complement S_N-A will have a unique
sum as well. Since sum(A) + sum(S_N-A) = sum(S_N), it is then easy to see
that sum(U(S_N, N/2)) is simply sum(S_N) times half the number of different
unique sums.

As an optimization, you only need to count how many unique sums there are
up to sum(S_N) / 2.
-}
