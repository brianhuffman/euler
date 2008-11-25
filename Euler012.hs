module Euler012 where
import Primes

------------------------------------------------------------------------------
-- 12. What is the first triangle number to have over five-hundred divisors?
{-
num_divisors n =
  product $
  map (1+) $
  map (flip exponent_of n) $
  takeWhile (<n) primes
-}

prob12a m n = head ys
  where (xs, ys) = span ((<= n) . num_divisors' m) (drop 1 triangle_seq)

triangle_seq = scanl (+) 0 [1 ..]

prob12 :: Int -> Integer
prob12 = prob12a 100
-- prob12 500 = 76576500
-- prob12 1000 = 842161320

-- triangle 12375 = 76576500
-- 12375 = 3 * 3 * 5 * 5 * 5 * 11
-- 12376 = 2 * 2 * 2 * 7 * 13 * 17

-- triangle 41040 = 842161320
-- 41040 = 2 2 2 2 3 3 3 5 19
-- 41041 = 7 11 13 41

main :: IO String
main = return $ show $ prob12 500
