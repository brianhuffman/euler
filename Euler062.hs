module Euler062 where
import Data.List

------------------------------------------------------------------------------
-- 62. Find the smallest cube for which exactly five permutations of its digits are cube.

prob62a =
  map head $
  filter ((==5) . length) $
  group $ sort $
  map (sort . show) $
  map (^3) [1 .. 10000]
-- "012334556789"
-- "012334566789"

main :: IO String
main = return $ show $ head $
  filter (\n -> sort (show n) `elem` prob62a) $
  map (^3) [1 .. 10000]
-- 127035954683

