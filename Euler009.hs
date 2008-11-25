module Euler009 where
import Pythagorean

------------------------------------------------------------------------------
-- 9. Find the only Pythagorean triplet, {a, b, c}, for which a + b + c = 1000.

prob9a n =
  [ (a, b, c) |
    (a, b, c) <- pythagorean_triples n,
    a + b + c == n ]
{-
is_pythag_triple a b c = a^2 + b^2 == c^2

prob9a =
  [(a,b,c) |
    a <- [1..1000],
    b <- [a..1000 - a],
    c <- [1000-a-b],
    is_pythag_triple a b c]
-- [(200,375,425)]
-}
prob9 n = a * b * c
  where (a,b,c) = head $ prob9a n

-- prob9a 1000 = [(200,375,425)]
-- prob9 1000 = 31875000

main :: IO String
main = return $ show $ prob9 1000
