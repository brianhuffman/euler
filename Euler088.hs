module Euler088 where
import EulerLib
import Primes
import Data.Array
import Data.List (nub, sort)
import qualified SortedList as S

------------------------------------------------------------------------------
-- 88. Exploring minimal product-sum numbers for sets of different sizes.
{-
A natural number, N, that can be written as the sum and product of a given set
of at least two natural numbers, {a1, a2, ... , ak} is called a product-sum
number: N = a1 + a2 + ... + ak = a1 × a2 × ... × ak.

For example, 6 = 1 + 2 + 3 = 1 × 2 × 3.

For a given set of size, k, we shall call the smallest N with this property a
minimal product-sum number. The minimal product-sum numbers for sets of size,
k = 2, 3, 4, 5, and 6 are as follows.

k=2: 4 = 2 × 2 = 2 + 2
k=3: 6 = 1 × 2 × 3 = 1 + 2 + 3
k=4: 8 = 1 × 1 × 2 × 4 = 1 + 1 + 2 + 4
k=5: 8 = 1 × 1 × 2 × 2 × 2 = 1 + 1 + 2 + 2 + 2
k=6: 12 = 1 × 1 × 1 × 1 × 2 × 6 = 1 + 1 + 1 + 1 + 2 + 6

Hence for 2 <= k <= 6, the sum of all the minimal product-sum numbers is
4+6+8+12 = 30; note that 8 is only counted once in the sum.

In fact, as the complete set of minimal product-sum numbers for 2 <= k <= 12
is {4, 6, 8, 12, 15, 16}, the sum is 61.

What is the sum of all the minimal product-sum numbers for 2 <= k <= 12000?
-}

{-
Replacing product [ab] with [a, b] increments k by (a-1)(b-1).
-}

-- array element n lists all possible values of k.
-- within the range n/2 <= k < n.
possible_sizes :: Int -> Array Int [Int]
possible_sizes m = arr
  where
    arr = funArray (1, m) sizes
    sizes 1 = []
    sizes n = foldl S.union []
      [ map (+i) (1 : arr ! a) |
        a <- tail (list_divisors n),
        a < n,
        let b = n `div` a,
        let i = (a-1)*(b-1) ]

-- array element k gives minimal n.
minimal_product_sum :: Int -> Array Int Int
minimal_product_sum m = arr
  where
    arr = accumArray min (2*m) (2, m) updates
    sizes = possible_sizes (2*m)
    updates = [ (k, n) | (n, ks) <- assocs sizes, k <- ks, k <= m ]

prob88 :: Int -> [Int]
prob88 m = nub $ sort $ elems $ minimal_product_sum m

main :: IO String
main = return $ show $ sum $ prob88 12000
-- 7587457
