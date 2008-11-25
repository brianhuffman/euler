module Euler032 where
import List (nub, sort)

------------------------------------------------------------------------------
-- 32. Find the sum of all numbers that can be written as pandigital products.

-- aa * aaa = aaaa
-- a * aaaa = aaaa

prob32a :: [(Int, Int, Int)]
prob32a =
  [ (a,b,a*b) |
    a <- [1 .. 9],
    b <- [1000 .. 9999`div`a],
    sort (concatMap show [a,b,a*b]) == "123456789" ] ++
  [ (a,b,a*b) |
    a <- [10 .. 99],
    b <- [100 .. 9999`div`a],
    sort (concatMap show [a,b,a*b]) == "123456789" ]
-- 4*1738=6952
-- 4*1963=7852
-- 12*483=5796
-- 18*297=5346
-- 27*198=5346
-- 28*157=4396
-- 39*186=7254
-- 42*138=5796
-- 48*159=7632

main :: IO String
main = return $ show $ sum $ nub $ map (\(_,_,c) -> c) prob32a
-- 45228

