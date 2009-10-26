module Euler235 where
import EulerLib (showFloat)

{-
Problem 235
An Arithmetic Geometric Sequence

07 March 2009

Given is the arithmetic-geometric sequence u(k) = (900-3k)r^(k-1).
Let s(n) = Σ_(k=1...n)u(k).

Find the value of r for which s(5000) = -600,000,000,000.

Give your answer rounded to 12 places behind the decimal point.

-}


{-
s(1) = 897
s(2) = 897 + 894 r
s(3) = 897 + 894 r + 891 r^2
s(4) = 897 + 894 r + 891 r^2 + 888 r^3
s(5) = 897 + 894 r + 891 r^2 + 888 r^3 + 885 r^4
s(6) = 897 + 894 r + 891 r^2 + 888 r^3 + 885 r^4 + 882 r^5
...
s(n) = 897 + 894 r + 891 r^2 + 888 r^3 + 885 r^4 + ... (900-3n)r^(n-1)

(1-r) s(1) = 897 - 897 r
(1-r) s(2) = 897 - 3 r - 894 r^2
(1-r) s(3) = 897 - 3 r - 3 r^2 - 891 r^3
(1-r) s(4) = 897 - 3 r - 3 r^2 - 3 r^3 - 888 r^4
...
(1-r) s(n) = 897 - 3 r - 3 r^2 - 3 r^3 - ... - 3 r^(n-1) - (900-3n)r^n

(1-r) s(n) = 900 - 3 (SUM i=0..n-1, r^i) - (900-3n)r^n
(1-r) s(n) = 900 - 3 ((1 - r^n) / (1 - r)) - (900-3n)r^n
(1-r) s(n) = 900 - 3 ((1 - r^n) / (1 - r)) - 900 r^n + 3n r^n
(1-r) s(n) = 900(1-r^n) - 3(1-r^n)/(1-r) + 3n(r^n)



For r = 1, s(5000) = -33007500

For r = 1.002322108, s(5000) = -5.99998252807332e11

For r = 1.0023221086328760742, s(5000) = -5.999999999993763e11
-}

u :: (Num a) => Int -> a -> a
u k r = (900 - 3 * fromIntegral (k::Int)) * r ^ (k - 1)

s :: (Num a) => Int -> a -> a
s n r = sum [ u k r | k <- [1 .. n] ]

s' :: (Fractional a) => Int -> a -> a
s' n r = (900*(1-r^n) - 3*(1-r^n)/(1-r) + 3*(fromIntegral n)*(r^n)) / (1-r)

x :: Double
x = -6e11

search :: Double -> Double -> Double
search x z
  | abs (x - z) < eps = x
  | s' 5000 y < -6e11 = search x y
  | otherwise = search y z
  where
    y = (x + z) / 2
    eps = 1e-15  

prob235 :: Double
prob235 = search 1 1.01

main :: IO String
main = return $ showFloat 12 prob235

answer :: String
answer = "1.002322108633"
