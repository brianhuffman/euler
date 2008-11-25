module Euler027 where
import EulerLib
import Primes

------------------------------------------------------------------------------
-- 27. Find a quadratic formula that produces the maximum number of primes for consecutive values of n.

increasingBy f m [] = []
increasingBy f z (x:xs) =
  let y = f x in
    if y < z then increasingBy f z xs
      else x : increasingBy f y xs

primes_for_quadratic a b =
  length $
  takeWhile is_prime $
  takeWhile (>1) $
  map (\n -> square n + a * n + b) [0..]

prob27a n = increasingBy thd3 0
  [ (a, b, primes_for_quadratic a b) |
    b <- takeWhile (<n) primes,
    a <- [-(n-1),-(n-3) .. (n-1)] ]
-- n^2 - 61n + 971 --> 71 primes

prob27 :: Int -> Int
prob27 n = a * b
  where (a,b,_) = last (prob27a n)
-- prob27 1000 = -59231  (6 seconds)

main :: IO String
main = return $ show $ prob27 1000
