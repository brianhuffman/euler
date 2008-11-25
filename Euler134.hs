module Euler134 where
import EulerLib
import Primes

------------------------------------------------------------------------------
-- 134. Finding the smallest positive integer related to any pair of consecutive primes.
{-
Consider the consecutive primes p1 = 19 and p2 = 23. It can be verified that
1219 is the smallest number such that the last digits are formed by p1 whilst
also being divisible by p2.

In fact, with the exception of p1 = 3 and p2 = 5, for every pair of consecutive
primes, p2 > p1, there exist values of n for which the last digits are formed
by p1 and n is divisible by p2. Let S be the smallest of these values of n.

Find S for every pair of consecutive primes with 5 <= p1 <= 1000000.
-}

-- divide_mod n a b = k such that b*k == a (mod 10^n)
divide_mod :: Int -> Int -> Int -> Int
divide_mod n a b = f n a
  where
    d = case b `mod` 10 of {1 -> 1; 3 -> 7; 7 -> 3; 9 -> 9}
    f 0 a = 0
    f n a = 10 * f (n-1) a' + c
      where
        c = (a * d) `mod` 10
        a' = (a - b * c) `div` 10

last_digits_divisible :: Int -> Int -> Int -> Integer
last_digits_divisible n a b =
  toInteger (divide_mod n a b) * toInteger b

-- (length p1, p1, p2)
prime_pairs :: [(Int, Int, Int)]
prime_pairs = chunk 1 pps0
  where
    pps0 = drop 2 $ zip primes (tail primes)
    chunk n xs = [ (n,p,q) | (p,q) <- ys ] ++ chunk (n+1) zs
      where (ys, zs) = span (\(p,q) -> p <= 10^n) xs

prob134 :: Int -> [Integer]
prob134 m =
  [ last_digits_divisible n a b |
    (n, a, b) <- takeWhile (\(n,a,b) -> a <= m) prime_pairs ]

main :: IO String
main = return $ show $ sum $ prob134 (10^6)
-- 18613426663617118
