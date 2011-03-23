module Euler290 where
import Data.Char (digitToInt)
import qualified Data.Map as M
import Data.Map (Map)

{-

Problem 290
Digital Signature
30 April 2010

How many integers 0 ≤ n < 10^18 have the property that the sum of the
digits of n equals the sum of digits of 137n?

-}

digit_sum :: Int -> Int
digit_sum = sum . map digitToInt . show

test :: [Int]
test = [ n | n <- [0..], digit_sum n == digit_sum (137*n) ]

{-

0     0   0
1   137  11
2   274  13
3   411   6
4   548  17
5   685  19
6   822  12
7   959  23
8  1096  16
9  1233   9

Dynamic programming solution:

Table indexed by 0 <= k <= 18.

For each k, keep a list/table/finite map of pairs:
  ((137n`div`10^k, dsum(n) - dsum(137n`mod`10^k)), multiplicity).

To calculate entry k+1:

going from n<10^k to m = n + a*10^k

dsum(m) = dsum(n) + a.


Let (x,r) = 137n `divMod` 10^k
so 137n = x*10^k + r.

Let m = d*10^k + n
137m = 137d*10^k + 137n
     = 137d*10^k + x*10^k + r.
     = (137d+x)*10^k + r.

Let (y,a) = (137d+x) `divMod` 10
137m = (10*y+a)*10^k + r.
137m = y*10^(k+1) + (a*10^k + r).

By adding a new digit d, we increment the digit sum of the tail of
137n by a, and replace the head of 137n by y.

At the same time, we increment the digit sum of n by d.

-}

type Bag a = Map a Integer

mapBag :: (Ord b) => (a -> b) -> Bag a -> Bag b
mapBag = M.mapKeysWith (+)

unionsBag :: (Ord a) => [Bag a] -> Bag a
unionsBag = M.unionsWith (+)

emptyBag :: Bag a
emptyBag = M.empty

insertBag :: (Ord a) => a -> Bag a -> Bag a
insertBag x = M.insert x 1

----

initBag :: Bag (Int, Int)
initBag = insertBag (0, 0) emptyBag

addDigit :: Int -> (Int, Int) -> (Int, Int)
addDigit d (x, r) = (y, r+d-a)
  where (y, a) = (137*d+x) `divMod` 10

nextBag :: Bag (Int, Int) -> Bag (Int, Int)
nextBag b = unionsBag [ mapBag (addDigit d) b | d <- [0..9] ]

nthBag :: Int -> Bag (Int, Int)
nthBag n = iterate nextBag initBag !! n

score :: Bag (Int, Int) -> Integer
score = M.foldWithKey f 0
  where f (x, r) m s = if digit_sum x == r then m+s else s

prob290 :: Int -> Integer
prob290 n = score (nthBag n)

main :: IO String
main = return $ show $ prob290 18

answer :: String
answer = "20444710234716473"
