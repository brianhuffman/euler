module Euler027 where
import EulerLib
import Primes

{-
Problem 27
27 September 2002

Euler published the remarkable quadratic formula:

    n² + n + 41

It turns out that the formula will produce 40 primes for the
consecutive values n = 0 to 39. However, when n = 40, 40² + 40 + 41 =
40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² +
41 + 41 is clearly divisible by 41.

Using computers, the incredible formula n² − 79n + 1601 was
discovered, which produces 80 primes for the consecutive values n = 0
to 79. The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

    n² + an + b, where |a| < 1000 and |b| < 1000

    where |n| is the modulus/absolute value of n
    e.g. |11| = 11 and |−4| = 4

Find the product of the coefficients, a and b, for the quadratic
expression that produces the maximum number of primes for consecutive
values of n, starting with n = 0.
-}

type Z = Int

increasingBy f m [] = []
increasingBy f z (x:xs) =
  let y = f x in
    if y < z then increasingBy f z xs
      else x : increasingBy f y xs

primes_for_quadratic :: Z -> Z -> Int
primes_for_quadratic a b =
  length $
  takeWhile primeInt $
  takeWhile (>1) $
  map (\n -> square n + a * n + b) [0..]

prob27a :: Z -> [(Z, Z, Int)]
prob27a n = increasingBy thd3 0
  [ (a, b, primes_for_quadratic a b) |
    b <- takeWhile (< n) primes,
    a <- [-(n-1),-(n-3) .. (n-1)] ]
-- n^2 - 61n + 971 --> 71 primes

prob27 :: Int -> Z
prob27 n = a * b
  where (a,b,_) = last (prob27a n)

main :: IO String
main = return $ show $ prob27 1000

answer :: String
answer = "-59231"
