module Euler053 where
import EulerLib

------------------------------------------------------------------------------
-- 53. How many values of C(n,r), for 1 ≤ n ≤ 100, exceed one-million?

prob53 n m = length $ filter (> m) $ concat (take (n+1) pascal_triangle)

main :: IO String
main = return $ show $ prob53 100 (10^6)
-- 4075


