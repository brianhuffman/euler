module Euler254 where
import Data.Char (digitToInt)
import Data.Array.Unboxed

{-
Problem 254
Sum of Digit Factorials

04 September 2009

Define f(n) as the sum of the factorials of the digits of n. For
example, f(342) = 3! + 4! + 2! = 32.

Define sf(n) as the sum of the digits of f(n). So sf(342) = 3 + 2 = 5.

Define g(i) to be the smallest positive integer n such that sf(n) =
i. Though sf(342) is 5, sf(25) is also 5, and it can be verified that
g(5) is 25.

Define sg(i) as the sum of the digits of g(i). So sg(5) = 2 + 5 = 7.

Further, it can be verified that g(20) is 267 and ∑ sg(i) for 1 ≤ i ≤
20 is 156.

What is ∑ sg(i) for 1 ≤ i ≤ 150?

-}

{-

g(150) = Least i such that sf(i) = 150.
f(i) must have more than 150/9 = 16 digits.
(i) must have at least 10^16 / 9! = 2.75e10 digits

This is much too big for type Int!
f(n) must be represented with a larger type.

-}

type Digit = Int

type N = [(Digit, Z)]
-- (digit, count) for representing (n).

type S = Integer
-- for representing sums of digits of n.

type Z = Integer
-- for representing sums of factorials of digits of n.

type I = Int
-- for sums of digits of type Z

------------------------------------------------------------
-- factorial

fact_arr :: UArray Digit Int
fact_arr = listArray (0,9) (scanl (*) 1 [1 .. 9])

fac :: Digit -> Z
fac n = fromIntegral (fact_arr ! n)

------------------------------------------------------------
-- f and its inverse

f :: N -> Z
f n = sum [ fromIntegral k * fac d | (d, k) <- n ]

{-

Finding smaller n with the same f(n).

* Permutations have the same result, so digits are always sorted.

* f(11) = f(2). Can be at most one 1.
* f(222) = f(3). Can be at most two 2s.
* f(3333) = f(4). Can be at most three 3s.
* f(44444) = f(5). Can be at most four 4s.
* f(555555) = f(6). Can be at most five 5s.
* f(6666666) = f(7). Can be at most six 6s.
* f(77777777) = f(8). Can be at most seven 7s.
* f(888888888) = f(9). Can be at most eight 8s.

-}

-- f_inv(i) = least n such that f(n) = i

f_inv :: Z -> N
f_inv i = reverse (go i 9)
  where
    go i 0 = []
    go i d = (d, fromIntegral q) : go r (d-1)
      where (q, r) = i `divMod` fac d

------------------------------------------------------------
-- sum of digits

sum_of_digits_array :: UArray Int I
sum_of_digits_array = listArray (0, 999)
  [ sum (map digitToInt (show n)) | n <- [0 .. 999] ]

sum_of_digits :: Z -> I
sum_of_digits n | n < 1000 = sum_of_digits_array!(fromIntegral n)
sum_of_digits n =
  sum_of_digits q + sum_of_digits_array!(fromIntegral r)
  where (q, r) = n `divMod` 10

{-
digSum :: Z -> I
--digSum = sum . map digitToInt . show
digSum 0 = 0
digSum n = digSum q + r
  where (q, r) = n `divMod` 10
-}

-- least z such that sum_of_digits z = i

s_inv :: I -> Z
s_inv i
  | i < 10 = fromIntegral i
  | otherwise = 9 + 10 * s_inv (i - 9)

------------------------------------------------------------
-- sf

sf :: N -> I
sf n = sum_of_digits (f n)

sumN :: N -> S
sumN n = sum [ fromIntegral d * k | (d, k) <- n ]

------------------------------------------------------------

------------------------------------------------------------

------------------------------------------------------------
-- g(i) is always a number with digits in ascending order.
-- sf preserves numbers modulo 9

{-
ascending :: Int -> Int -> [N]
ascending l 9 = [[(9, l)]]
ascending l d =
  [ (d, n) : xs |
    n <- reverse [0 .. min l d],
    xs <- ascending (l-n) (d+1)
  ]

-- ascending_mod r l d =
--   [ xs <- ascending l d | sumN xs `mod` 9 == r ] 

ascending_mod :: Int -> Int -> Int -> [N]
ascending_mod 0 l 9 = [[(9, l)]]
ascending_mod r l d
  | d > 2 && r `mod` 3 /= 0 = []
  | d > 5 && r `mod` 9 /= 0 = []
  | otherwise =
      [ (d, n) : xs |
        n <- reverse [0 .. min l d],
        let r' = (r - n * fromIntegral (fac d)) `mod` 9,
        let l' = l - n,
        xs <- ascending_mod r' l' (d+1)
      ]

all_ascending_mod :: Int -> [N]
all_ascending_mod r =
  [ xs |
    l <- [1 ..],
    xs <- ascending_mod r l 1
  ]

-}

ascending_mod_fs :: Int -> Int -> Int -> [(Z, S)]
ascending_mod_fs 0 l 9 = [(fromIntegral l * fac 9, fromIntegral l * 9)]
ascending_mod_fs r l d
  | d > 2 && r `mod` 3 /= 0 = []
  | d > 5 && r `mod` 9 /= 0 = []
  | otherwise =
      [ (f' + fromIntegral n * fac d, s' + fromIntegral (n * d)) |
        n <- reverse [0 .. min l d],
        let r' = (r - n * fromIntegral (fac d)) `mod` 9,
        let l' = l - n,
        (f',s') <- ascending_mod_fs r' l' (d+1)
      ]

all_ascending_mod_fs :: Int -> [(Z, S)]
all_ascending_mod_fs r =
  [ xs |
    l <- [1 ..],
    xs <- ascending_mod_fs r l 1
  ]

------------------------------------------------------------
-- g and sg functions

{-
g :: I -> N
g i = head [ ds | ds <- all_ascending_mod r, sf ds == i ]
  where r = i `mod` 9
-}

sg :: I -> (Z, S)
--sg = sumN . g
sg i = head
  [ (fn, sn) |
    (fn, sn) <- all_ascending_mod_fs r,
    sum_of_digits fn == i
  ]
  where r = i `mod` 9

test = [ (i, s) | i <- [1 ..], let s = sg i ]

big_sg :: I -> S
big_sg i = sumN (f_inv (s_inv i))
-- correct for large i (> 62)

prob254 :: S
prob254 =
  sum [ snd (sg i) | i <- [1 .. 62] ] +
  sum [ big_sg i | i <- [63 .. 150] ]

main :: IO String
main = return $ show prob254

answer :: String
answer = "8184523820510"
