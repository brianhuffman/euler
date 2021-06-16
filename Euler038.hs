module Euler038 where
import Data.List (sort)

------------------------------------------------------------------------------
-- 38. What is the largest 1 to 9 pandigital that can be formed by multiplying a fixed number by 1, 2, 3, ... ?

prob38a :: [String]
prob38a =
  filter (\s -> sort s == "123456789") $
  map (\n -> show n ++ show (n*2)) [9000 .. 9999]

main :: IO String
main = return $ last prob38a
-- 932718654

