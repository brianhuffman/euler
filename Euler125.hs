module Euler125 where
import EulerLib
import qualified SortedList as S
import List

------------------------------------------------------------------------------
-- 125. Finding square sums that are palindromic.
{-
The palindromic number 595 is interesting because it can be written as the
sum of consecutive squares: 6^2 + 7^2 + 8^2 + 9^2 + 10^2 + 11^2 + 12^2.

There are exactly eleven palindromes below one-thousand that can be written
as consecutive square sums, and the sum of these palindromes is 4164. Note
that 1 = 0^2 + 1^2 has not been included as this problem is concerned with
the squares of positive integers.

Find the sum of all the numbers less than 10^8 that are both palindromic and
can be written as the sum of consecutive squares.
-}

{-
sum (map square [0 .. n]) = (2n^3 + 3n^2 + n) / 6
                          = n * (n+1) * (2n+1) / 6

sum (map square [a+1 .. b]) =
  sum (map square [0 .. b]) - sum (map square [0 .. a])

= (2b^3 + 3b^2 + b) / 6  -  (2a^3 + 3a^2 + a) / 6
= (2b^3 + 3b^2 + b - 2a^3 - 3a^2 - a) / 6
= (2(b^3 - a^3) + 3(b^2 - a^2) + b - a) / 6
= (2(b-a)(b^2 + ba + a^2) + 3(b-a)(b+a) + (b-a)) / 6
= (b-a) (2(b^2 + ba + a^2) + 3(b+a) + 1) / 6
= (b-a) (2(b^2 + ba + a^2) + 3(b+a) + 1) / 6

(a+k)^2 + (a-k)^2
= (a^2 + 2ak + k^2) + (a^2 - 2ak + k^2)
= 2a^2 + 2k^2

sum [ n^2 | n <- [a-1 .. a+1] ] = 3*a^2 + 2
sum [ n^2 | n <- [a-2 .. a+2] ] = 5*a^2 + 10
sum [ n^2 | n <- [a-3 .. a+3] ] = 7*a^2 + 28
sum [ n^2 | n <- [a-4 .. a+4] ] = 9*a^2 + 60

sum [ n^2 | n <- [a .. b] ]

-}

-- sums of consecutive squares
sum_squares :: [Integer]
sum_squares = S.big_union yss
  where
    squares = map square [1 ..]
    xs = scanl (+) 0 squares
    xss = drop 2 (tails xs)
    yss = map (zipWith subtract xs) xss

palindromes :: [Integer]
palindromes =
  [ read (s ++ t) |
    l <- [1 ..],
    b <- [True, False],
    n <- [10^(l-1) .. (10^l)-1],
    let s = show n,
    let r = reverse s,
    let t = if b then tail r else r ]

sum_square_palindromes :: [Integer]
sum_square_palindromes = filter (palindrome . show) sum_squares

prob125c :: Integer -> [Integer]
prob125c m = takeWhile (< m) sum_square_palindromes

prob125 :: Integer -> Integer
prob125 m = sum (prob125c m)

main :: IO String
main = return $ show $ prob125 (10^8)
-- 2906969179

-- FIXME: SLOW