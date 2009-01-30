module EulerLib where
import Primes
import Data.Array
import Data.List

divides x y = y `mod` x == 0

palindrome s = s == reverse s
fibs = zipWith (+) (0 : fibs) (0 : 1 : fibs)
square x = x * x

sieve p xs = filter (not . divides p) xs

{-
funArray :: (IArray a e, Ix i) => (i, i) -> (i -> e) -> a i e
funArray ij f = listArray ij (map f (range ij))
-}

funArray :: (Ix i) => (i, i) -> (i -> e) -> Array i e
funArray ij f = listArray ij (map f (range ij))

{-
memoize :: (Ix i) => (i, i) -> (i -> e) -> i -> e
memoize ij f = g
  where
    a = listArray ij (map f (range ij))
    g x = if inRange ij x then a ! x else f x
-}

triangle_seq = scanl1 (+) [0..]

factorial n = if n <= 0 then 1 else n * factorial (n - 1)

counts :: (Eq a) => [a] -> [(a, Int)]
counts = map (\xs -> (head xs, length xs)) . group

deltas xs = zipWith subtract xs (tail xs)

sum' :: (Num a) => [a] -> a
sum' = foldl' (+) 0

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1' max

minimum' :: (Ord a) => [a] -> a
minimum' = foldl1' min

-- rspan p xs = let (ys,zs) = span p xs in (rev ys, zs)
rspan p = f []
  where
    f xs [] = (xs,[])
    f xs (y:ys) = if p y then f (y:xs) ys else (xs, y:ys)

fst3 (a,b,c) = a
snd3 (a,b,c) = b
thd3 (a,b,c) = c

chunk n [] = []
chunk n xs = ys : chunk n zs
  where (ys,zs) = splitAt n xs

triangle x = (x * (x + 1)) `div` 2

middle Nothing Nothing = 0
middle (Just a) Nothing = 2 * a + 1
middle Nothing (Just b) = 2 * b - 1
middle (Just a) (Just b) = (a + b) `div` 2

binary_search f = between Nothing Nothing
  where
    between a b =
      let x = middle a b in
      if a == Just x then Left x else
      case f x of
        LT -> between a (Just x)
        EQ -> Right x
        GT -> between (Just x) b

-- square_root x = either id id $ binary_search (\n -> compare x (square n))
{-
square_root x = f x
  where
    f r = let r' = ((r + x `div` r) `div` 2)
          in if r <= r' then r else f r'
-}

sortOf f = sortBy (\x y -> compare (f x) (f y))
maximumOf f = maximumBy (\x y -> compare (f x) (f y))
minimumOf f = minimumBy (\x y -> compare (f x) (f y))

all_same [] = True
all_same (x:xs) = all (==x) xs

fibonacci n = fst (f n)
  where
    f 0 = (0, 1)
    f n
      | even n = (2*a*b - a^2, a^2 + b^2)
      | otherwise = (a^2 + b^2, 2*a*b + b^2)
      where (a, b) = f (n `div` 2)

pascal_triangle :: (Num a) => [[a]]
pascal_triangle = iterate f [1]
  where f xs = zipWith (+) ([0] ++ xs) (xs ++ [0])

choose :: (Num a) => Int -> Int -> a
n `choose` r = pascal_triangle !! n !! r

spiral_seq :: [Int]
spiral_seq = scanl (+) 1 $ concatMap (replicate 4) [2,4..]

distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs
