module Euler239 where
import EulerLib
import Data.Ratio

{-
Problem 239
Twenty-two Foolish Primes

03 April 2009

A set of disks numbered 1 through 100 are placed in a line in random
order.

What is the probability that we have a partial derangement such that
exactly 22 prime number discs are found away from their natural
positions?  (Any number of non-prime disks may also be found in or out
of their natural positions.)

Give your answer rounded to 12 places behind the decimal point in the
form 0.abcdefghijkl.


-}


{-
Primes below 100:
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

There are 25.

Exactly 22 must be out of place.

Exactly 3 must be in place.


-}

-- total number of arrangements
total :: Integer
total = factorial 100

-- total with all 25 in place
-- num25 = factorial 75

-- total with exactly 24/25 in place
-- 25 possibilities for which out of place
-- 75 possible positions for the 25th one
-- 75! arrangements for the remainder
-- num24 = 25 * 75 * factorial 75

n25 = factorial 75
n24 = factorial 76 - n25
n23 = factorial 77 - n25 - 2*n24
n22 = factorial 78 - n25 - 3*n24 - 3*n23
n21 = factorial 79 - n25 - 4*n24 - 6*n23 - 4*n22
n20 = factorial 80 - sum (zipWith (*) (pascal_triangle !! 5) [n25,n24,n23,n22,n21])
n19 = factorial 81 - sum (zipWith (*) (pascal_triangle !! 6) [n25,n24,n23,n22,n21,n20])
n18 = factorial 82 - sum (zipWith (*) (pascal_triangle !! 7) [n25,n24,n23,n22,n21,n20,n19])
n17 = factorial 83 - sum (zipWith (*) (pascal_triangle !! 8) [n25,n24,n23,n22,n21,n20,n19,n18])
n16 = factorial 84 - sum (zipWith (*) (pascal_triangle !! 9) [n25,n24,n23,n22,n21,n20,n19,n18,n17])
n15 = factorial 85 - sum (zipWith (*) (pascal_triangle !! 10) [n25,n24,n23,n22,n21,n20,n19,n18,n17,n16])
n14 = factorial 86 - sum (zipWith (*) (pascal_triangle !! 11) [n25,n24,n23,n22,n21,n20,n19,n18,n17,n16,n15])
n13 = factorial 87 - sum (zipWith (*) (pascal_triangle !! 12) [n25,n24,n23,n22,n21,n20,n19,n18,n17,n16,n15,n14])
n12 = factorial 88 - sum (zipWith (*) (pascal_triangle !! 13) [n25,n24,n23,n22,n21,n20,n19,n18,n17,n16,n15,n14,n13])
n11 = factorial 89 - sum (zipWith (*) (pascal_triangle !! 14) [n25,n24,n23,n22,n21,n20,n19,n18,n17,n16,n15,n14,n13,n12])
n10 = factorial 90 - sum (zipWith (*) (pascal_triangle !! 15) [n25,n24,n23,n22,n21,n20,n19,n18,n17,n16,n15,n14,n13,n12,n11])
n9 = factorial 91 - sum (zipWith (*) (pascal_triangle !! 16) [n25,n24,n23,n22,n21,n20,n19,n18,n17,n16,n15,n14,n13,n12,n11,n10])
n8 = factorial 92 - sum (zipWith (*) (pascal_triangle !! 17) [n25,n24,n23,n22,n21,n20,n19,n18,n17,n16,n15,n14,n13,n12,n11,n10,n9])
n7 = factorial 93 - sum (zipWith (*) (pascal_triangle !! 18) [n25,n24,n23,n22,n21,n20,n19,n18,n17,n16,n15,n14,n13,n12,n11,n10,n9,n8])
n6 = factorial 94 - sum (zipWith (*) (pascal_triangle !! 19) [n25,n24,n23,n22,n21,n20,n19,n18,n17,n16,n15,n14,n13,n12,n11,n10,n9,n8,n7])
n5 = factorial 95 - sum (zipWith (*) (pascal_triangle !! 20) [n25,n24,n23,n22,n21,n20,n19,n18,n17,n16,n15,n14,n13,n12,n11,n10,n9,n8,n7,n6])
n4 = factorial 96 - sum (zipWith (*) (pascal_triangle !! 21) [n25,n24,n23,n22,n21,n20,n19,n18,n17,n16,n15,n14,n13,n12,n11,n10,n9,n8,n7,n6,n5])
n3 = factorial 97 - sum (zipWith (*) (pascal_triangle !! 22) [n25,n24,n23,n22,n21,n20,n19,n18,n17,n16,n15,n14,n13,n12,n11,n10,n9,n8,n7,n6,n5,n4])
n2 = factorial 98 - sum (zipWith (*) (pascal_triangle !! 23) [n25,n24,n23,n22,n21,n20,n19,n18,n17,n16,n15,n14,n13,n12,n11,n10,n9,n8,n7,n6,n5,n4,n3])
n1 = factorial 99 - sum (zipWith (*) (pascal_triangle !! 24) [n25,n24,n23,n22,n21,n20,n19,n18,n17,n16,n15,n14,n13,n12,n11,n10,n9,n8,n7,n6,n5,n4,n3,n2])
n0 = factorial 100 - sum (zipWith (*) (pascal_triangle !! 25) [n25,n24,n23,n22,n21,n20,n19,n18,n17,n16,n15,n14,n13,n12,n11,n10,n9,n8,n7,n6,n5,n4,n3,n2,n1])

num24 = (25 `choose` 24) * n24
num23 = (25 `choose` 23) * n23
num22 = (25 `choose` 22) * n22
num21 = (25 `choose` 22) * n22
num20 = (25 `choose` 22) * n22
num19 = (25 `choose` 19) * n19
num18 = (25 `choose` 18) * n18
num17 = (25 `choose` 17) * n17
num16 = (25 `choose` 16) * n16
num15 = (25 `choose` 15) * n15
num14 = (25 `choose` 14) * n14
num13 = (25 `choose` 13) * n13
num12 = (25 `choose` 12) * n12
num11 = (25 `choose` 11) * n11
num10 = (25 `choose` 10) * n10
num3 = (25 `choose` 3) * n3

-- num23 = 25 `choose` 2 * 

-- num3 = 25 `choose` 3

prob239 :: Rational
prob239 = num3 % factorial 100

main :: IO String
main = return $ showFloat 12 $ prob239

answer :: String
answer = "0.001887854841"
