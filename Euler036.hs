module Euler036 where
import EulerLib
import qualified SortedList as S

------------------------------------------------------------------------------
-- 36. Find the sum of all numbers less than one million, which are palindromic in base 10 and base 2.

-- decimal_palindromes, in sorted order
decimal_palindromes :: [Integer]
decimal_palindromes =
  [ read (s ++ t) |
    l <- [1 ..],
    b <- [True, False],
    n <- [10^(l-1) .. (10^l)-1],
    let s = show n,
    let r = reverse s,
    let t = if b then tail r else r ]

show_binary :: Integer -> String
show_binary n = f "" n
  where
    f cs 0 = cs
    f cs n = f ((if even n then '0' else '1'):cs) (n `div` 2)

read_binary :: String -> Integer
read_binary = f 0
  where
    f n [] = n
    f n (d:ds) = f (2*n + if d == '0' then 0 else 1) ds

double_palindromes :: Integer -> [Integer]
double_palindromes m =
  filter (palindrome . show_binary) $
  takeWhile (< m) decimal_palindromes

prob36 :: Integer -> Integer
prob36 = sum . double_palindromes
-- prob36 1000000 = 872187

main :: IO String
main = return $ show $ prob36 (10^6)
