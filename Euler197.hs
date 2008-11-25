module Euler197 where

-- 197. Investigating the behaviour of a recursively defined sequence.
{-
Given is the function f(x) = floor(2^(30.403243784-x^2)) × 10-9,
the sequence u(n) is defined by u(0) = -1 and u(n+1) = f(u(n)).

Find u(n) + u(n+1) for n = 10^12.
Give your answer with 9 digits after the decimal point.
-}


{-
2^30.403243784 ~ 1,420,000,000

0 < f(x) <= 1.42

For positive x, f(x) has negative slope.

f has one (unstable) fixed point.
(f . f) has two other fixed points; sequence u converges to this pair.
-}

e1 = 1420000000
e2 = 1400294917
e3 = 1400000000

x0 :: Int
x0 = -1000000000

f :: Int -> Int -> Int
f e n = floor (fromIntegral e * 2 ** (- x^2))
  where x = fromIntegral n * 1e-9

us :: [Int]
us = iterate (f e1) x0

fixed_pair :: (Eq a) => (a -> a) -> a -> (a, a)
fixed_pair f x0 = p (iterate f x0)
  where
    p (x:xs@(y:z:_))
      | x == z = (x, y)
      | otherwise = p xs

prob197 e = show q ++ "." ++ show r
  where
    (x, y) = fixed_pair (f e) x0
    (q, r) = (x+y) `divMod` (10^9)

main :: IO String
main = return $ prob197 e1