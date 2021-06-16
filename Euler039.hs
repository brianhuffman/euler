module Euler039 where
import EulerLib
import Pythagorean
import Data.List

------------------------------------------------------------------------------
-- 39. If p is the perimeter of a right angle triangle, {a, b, c}, which value, for p ≤ 1000, has the most solutions?

prob39 :: Int -> Int
prob39 n =
  head $ maximumOf length $ group $ sort $
  map (\(a,b,c) -> a+b+c) $ pythagorean_triples n
-- prob39 1000 = 840

main :: IO String
main = return $ show $ prob39 1000
-- 840
