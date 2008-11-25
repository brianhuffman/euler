module Euler026 where
import EulerLib
import Primes

------------------------------------------------------------------------------
-- 26. Find the value of d < 1000 for which 1/d contains the longest recurring cycle.

{-
Note:
(cycleLength base d) always divides (totient d)
-}

powersMod :: Int -> Int -> [Int]
powersMod p n = iterate f 1
  where f x = (p * x) `mod` n

cycleLength :: Int -> Int -> Int
cycleLength base d =
  (+1) $ length $
  takeWhile (>1) $ drop 1 $
  powersMod base d

-- precondition: reverse-sorted by d
-- precondition: l < d
maxcycle :: (Int, Int) -> [(Int, Int)] -> (Int, Int)
maxcycle (d',l') [] = (d',l')
maxcycle (d',l') ((d,l):dls)
  | d < l' = (d',l')
  | l < l' = maxcycle (d',l') dls
  | otherwise = maxcycle (d,l) dls

prob26 :: Int -> Int -> Int
prob26 base n = fst $
  maxcycle (0,0) $
  map (\d -> (d, cycleLength base d)) ds
  where
    ds = filter (coprime base) [n, n-1 .. 2]
    coprime x y = gcd x y == 1
-- prob26 1000 = 983
-- prob26 10000 = 9967
-- prob26 100000 = 99989
-- prob26 1000000 = 999983
-- prob26 10000000 = 9999943

-------------------------------------------------------
-- version that scales much better
-- only looks for 1/d with cycle length = d-1

has_cycle_length_one_less base d =
  is_prime d &&
  all (\b -> expMod base b d /= 1) bs
  where
    bs = [ (d-1) `div` p | (p,e) <- prime_factorization (d-1) ]

prob26' :: Integer -> Integer -> Integer
prob26' base n = head $
  filter (has_cycle_length_one_less base) [n, n-1 ..]

main :: IO String
main = return $ show $ prob26' 10 1000
