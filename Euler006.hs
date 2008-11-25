module Euler006 where

------------------------------------------------------------------------------
-- 6. What is the difference between the sum of the squares and the square of the sums? (of 1..100)

square x = x * x

prob6 n = square (sum [1 .. n]) - sum (map square [1 .. n])

prob6' n = n * (n+1) * (n-1) * (3*n + 2) `div` 12

main :: IO String
main = return $ show $ prob6' 100
-- 25164150

{-
sum [1..n] = (n^2 + n) / 2

(sum [1..n])^2
= ((n^2 + n) / 2)^2
= (n^4 + 2n^3 + n^2) / 4

sum (map square [1 .. n]) =
(2n^3 + 3n^2 + n) / 6

square (sum [1..n]) - sum (map square [1..n])
= (n^4 + 2n^3 + n^2) / 4 - (2n^3 + 3n^2 + n) / 6
= (3n^4 + 6n^3 + 3n^2) / 12 - (4n^3 + 6n^2 + 2n) / 12
= (3n^4 + 2n^3 - 3n^2 - 2n) / 12
= (3n^2 + 2n) * (n^2 - 1) / 12

(n+1)^1 - n^1 = 1
(n+1)^2 - n^2 = 2n + 1
(n+1)^3 - n^3 = 3n^2 + 3n + 1
(n+1)^4 - n^4 = 4n^3 + 6n^2 + 4n + 1

n^1 - (n-1)^1 = 1
n^2 - (n-1)^2 = 2n - 1
n^3 - (n-1)^3 = 3n^2 - 3n + 1
n^4 - (n-1)^4 = 4n^3 - 6n^2 + 4n - 1

2n^3 - 2(n-1)^3 = 6n^2 - 6n + 2
3n^2 - 3(n-1)^2 = 6n - 3
n^1 - (n-1)^1 = 1
-}

