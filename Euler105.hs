module Euler105 where
import Permutation
import EulerLib (distinct)
import Data.List (sort)

------------------------------------------------------------------------------
-- 105. Find the sum of the special sum sets in the file.

special :: [Int] -> Bool
special xs = all p1 [1 .. l2] && p2
  where
    p1 n = distinct [ sum ys | ys <- subseqs_len n xs ]
    p2 = sum (take (l2 + 1) xs') >
         sum (take l2 (reverse xs'))
    p3 = distinct (map sum $ subsets xs) -- redundant
    l2 = length xs `div` 2
    xs' = sort xs

setsTxt :: IO [[Int]]
setsTxt = readFile "sets.txt" >>= return .
  map (\l -> read ("[" ++ l ++ "]")) . lines

main :: IO String
main = setsTxt >>= return . show . sum . map sum . filter special
-- 73702
