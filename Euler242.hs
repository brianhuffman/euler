module Euler242 where
import EulerLib (choose)

{-
Problem 242
Odd Triplets

25 April 2009

Given the set {1,2,...,n}, we define f(n,k) as the number of its
k-element subsets with an odd sum of elements. For example, f(5,3)
= 4, since the set {1,2,3,4,5} has four 3-element subsets having an
odd sum of elements, i.e.: {1,2,4}, {1,3,5}, {2,3,4} and {2,4,5}.

When all three values n, k and f(n,k) are odd, we say that they make
an odd-triplet [n,k,f(n,k)].

There are exactly five odd-triplets with n ≤ 10, namely:
[1,1,f(1,1) = 1], [5,1,f(5,1) = 3], [5,5,f(5,5) = 1], [9,1,f(9,1) = 5]
and [9,9,f(9,9) = 1].

How many odd-triplets are there with n ≤ 10^(12) ?

-}

{-

odd sum of elements <--> odd number of odd elements.
n = 4k+0 -> 2k even, 2k odd
n = 4k+1 -> 2k even, 2k+1 odd
n = 4k+2 -> 2k+1 even, 2k+1 odd
n = 4k+3 -> 2k+1 even, 2k+2 odd

odd (n choose k)


    0 1 2 3 4 5 6 7 8
  +------------------
0 | 1 0 0 0 0 0 0 0 0
1 | 1 1 0 0 0 0 0 0 0
2 | 1 0 1 0 0 0 0 0 0
3 | 1 1 1 1 0 0 0 0 0
4 | 1 0 0 0 1 0 0 0 0
5 | 1 1 0 0 1 1 0 0 0
6 | 1 0 1 0 1 0 1 0 0
7 | 1 1 1 1 1 1 1 1 0
8 | 1 0 0 0 0 0 0 0 1
  +------------------
    0 1 2 3 4 5 6 7 8

-}

-- odd_choose n k = odd (choose n k)
-- odd_choose :: Integer -> Integer -> Bool
odd_choose n 0 = True
odd_choose n k
  | even n && odd k = False
  | otherwise = odd_choose (n`div`2) (k`div`2)

-- choose even odd = even

-- f :: (Integer, Integer) -> Integer
f (n, k) = sum
  [ choose n_even k_even * choose n_odd k_odd | 
    k_odd <- [1,3 .. n_odd],
    let k_even = k - k_odd,
    k_odd <= k,
    k_even <= n_even
  ]
  where
    n_odd = (n+1) `div` 2
    n_even = n `div` 2

{-
n == 3 (mod 4)
--> n_odd is even
--> (choose n_odd k_odd) is even
--> f (n, k) is even.

-}

odd_f (n, k) = odd . length $
  [ k_odd |
    k_odd <- [1,3 .. min k n_odd],
    let k_even = k - k_odd,
    k_even <= n_even,
    odd_choose n_even k_even,
    odd_choose n_odd k_odd
  ]
  where
    n_odd = (n+1) `div` 2
    n_even = n `div` 2

odd_triplets n_max =
  [ (n, k) |
    n <- [1,5 .. n_max], -- only test n == 1 (mod 4)
    k <- [1,3 .. n],
    odd_f (n, k)
  ]

----

-- countBits :: Integer -> Int
powerBits 0 = 1
powerBits n
  | even n    = powerBits (n `div` 2)
  | otherwise = powerBits (n `div` 2) * 2

num_triplets n_max = sum
  [ powerBits m |
    m <- [0 .. (n_max-1)`div`4]
  ]

--------

-- powerBitsUpTo n = sum [ powerBits m | m <- [0..n] ]
powerBitsUpTo 0 = 1
powerBitsUpTo n
  | odd n = 3 * powerBitsUpTo (n `div` 2)
  | even n = 3 * powerBitsUpTo (n `div` 2) - powerBits (n + 1)

num_triplets' n_max = powerBitsUpTo ((n_max-1) `div` 4)

main :: IO String
main = return $ show $ num_triplets' (10^12)

answer :: String
answer = "997104142249036713"

{-

(n,1) forms an odd triplet when n == 1 (mod 4).
(n,n) forms an odd triplet when n == 1 (mod 4).



1: 1
101: 001 101

1001: 0001 1001

1101: 0001 0101 1001 1101

10001: 00001 10001

10101: 00001 00101 10001 10101

11001: 00001 01001 10001 11001

11101: 00001 00101 01001 01101 10001 10101 11001 11101

(4n+1, 4k+1) forms a triplet iff k has a subset of n's bits.

(1,1)
(5,1),(5,5)
(9,1),(9,9)
(13,1),(13,5),(13,9),(13,13)
(17,1),(17,17)
(21,1),(21,5),(21,17),(21,21)
(25,1),(25,9),(25,17),(25,25)
(29,1),(29,5),(29,9),(29,13),(29,17),(29,21),(29,25),(29,29)
(33,1),(33,33)
(37,1),(37,5),(37,33),(37,37)
(41,1),(41,9),(41,33),(41,41)
-}
