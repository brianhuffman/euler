module Euler104 where
import List (sort)

{-
Problem 104
Pandigital Fibonacci Numbers

09 September 2005

The Fibonacci sequence is defined by the recurrence relation:

    F_(n) = F_(n−1) + F_(n−2), where F_(1) = 1 and F_(2) = 1.

It turns out that F_(541), which contains 113 digits, is the first
Fibonacci number for which the last nine digits are 1-9 pandigital
(contain all the digits 1 to 9, but not necessarily in order). And
F_(2749), which contains 575 digits, is the first Fibonacci number for
which the first nine digits are 1-9 pandigital.

Given that F_(k) is the first Fibonacci number for which the first
nine digits AND the last nine digits are 1-9 pandigital, find k.
-}

-- fibonacci sequence is periodic (mod 10), with period 60.
-- every 15th value is a multiple of 10.

fibs_mod :: Int -> [Int]
fibs_mod n = f 0 1
  where
    reduce x = if x < n then x else x - n
    f a b = a : f b (reduce (a+b))

-- fibs_msb n = 20 most significant digits of fibonacci(n)
fibs_msd :: Int -> Integer
fibs_msd n = fst (f n)
  where
    d = 20 -- number of digits
    -- f n = (a, b) implies that 10^(d-1) <= a < 10^d
    f 1 = (10^(d-1), 10^(d-1))
    f n
      | even n = reduce (2*a*b - a^2, a^2 + b^2)
      | otherwise = reduce (a^2 + b^2, 2*a*b + b^2)
      where (a, b) = f (n `div` 2)
    reduce (a, b)
      | a < 10^(2*d-2) = error "term got too small"
      | a < 10^(2*d-1) = (a `div` 10^(d-1), b `div` 10^(d-1))
      | a < 10^(2*d)   = (a `div` 10^d    , b `div` 10^d)
      | a < 10^(2*d+1) = (a `div` 10^(d+1), b `div` 10^(d+1))
      | otherwise = error "term got too big"

prob104 :: [Int]
prob104 =
  [ i |
    (n, i) <- zip (fibs_mod (10^9)) [0 ..],
    sort (show n) == "123456789",
    sort (take 9 (show (fibs_msd i))) == "123456789" ]

main :: IO String
main = return $ show $ head prob104

answer :: String
answer = "329468"

-- old, simple versions:

last9fibs = f 0 1 where
  m = 10^9
  reduce x = if x < m then x else x - m
  f a b = a : f b (reduce (a+b))

first9fibs = f 0 1 where
  f a b
    | a < 10^20 = a : f b (a+b)
    | otherwise = f (a `div` 10) (b `div` 10)
