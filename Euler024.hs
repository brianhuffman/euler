module Euler024 where
import EulerLib

------------------------------------------------------------------------------
-- 24. What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

-- # of permutations = factorial 10 = 3628800

remove_nth :: Int -> [a] -> (a, [a])
remove_nth _ [] = error "remove_nth: out of bounds"
remove_nth n (x:xs)
  | n == 0 = (x, xs)
  | otherwise = let (y, ys) = remove_nth (n-1) xs in (y, x:ys)

nth_permutation :: Int -> [a] -> [a]
nth_permutation n [] = []
nth_permutation n xs = y : nth_permutation r ys
  where
    (q,r) = n `divMod` (factorial (length xs - 1))
    (y,ys) = remove_nth q xs

main :: IO String
main = return $ nth_permutation (1000000 - 1) "0123456789"

