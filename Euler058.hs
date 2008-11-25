module Euler058 where
import EulerLib
import Primes
import qualified SortedList as S

------------------------------------------------------------------------------
-- 58. Investigate the number of primes that lie on the diagonals of the spiral grid.
{-
 37 36 35 34 33 32 31
 38 17 16 15 14 13 30
 39 18  5  4  3 12 29
 40 19  6  1  2 11 28
 41 20  7  8  9 10 27
 42 21 22 23 24 25 26
 43 44 45 46 47 48 49

lower-right: a(n) = (2n+1)^2      = 4n^2 + 4n + 1
lower-left:  b(n) = (2n+1)^2 - 2n = 4n^2 + 2n + 1
upper-left:  c(n) = (2n+1)^2 - 4n = 4n^2 + 1
upper-right: d(n) = (2n+1)^2 - 6n = 4n^2 - 2n + 1

-}
spiral_primes = S.intersect primes spiral_seq
spiral_prime_ratio n = (n, x, 2*n-1)
  where x = length $ takeWhile (<= square n) spiral_primes

-- (edge length, num primes, num diagonal elements)
prob58a ps = span t (drop 1 xs)
  where
    t (_,x,y) = y <= 10*x
    xs = accum 1 0 spiral_primes
    spiral_primes = filter (divides_none ps) (drop 1 spiral_seq)
    accum n count [] = []
    accum n count (p:ps)
      | p <= square n = accum n (count+1) ps
      | otherwise = (n, count, 2*n-1) : accum (n+2) count (p:ps)

main :: IO String
main = return $ show $ fst3 $ head $ snd $ prob58a primes
-- 26241

