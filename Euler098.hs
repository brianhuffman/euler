module Euler098 where
import Permutation
import EulerLib (square, distinct)
import Data.Maybe
import Data.List
import qualified Data.Set as Set

{-
Problem 98
Investigating words, and their anagrams, which can represent square numbers.

17 June 2005

By replacing each of the letters in the word CARE with 1, 2, 9, and 6
respectively, we form a square number: 1296 = 36^2. What is remarkable is that,
by using the same digital substitutions, the anagram, RACE, also forms a square
number: 9216 = 96^2. We shall call CARE (and RACE) a square anagram word pair
and specify further that leading zeroes are not permitted, neither may a
different letter have the same digital value as another letter.

Using words.txt, a 16K text file containing nearly two-thousand common English
words, find all the square anagram word pairs (a palindromic word is NOT
considered to be an anagram of itself).

What is the largest square number formed by any member of such a pair?

NOTE: All anagrams formed must be contained in the given text file.
-}

-- N.B.: none of the anagrams happen to have repeated letters

-- sorted, with longest anagrams first
anagrams :: [String] -> [[String]]
anagrams ws =
  sortBy (\x y -> compare (length (head y)) (length (head x))) $
  map (map snd) $
  filter (not . null . drop 1) $
  groupBy (\(x,_) (y,_) -> x == y) $
  sort [ (sort w, w) | w <- ws ]

prob98 anas = filter ok squares'
  where
    squares = map (show . square) [1 .. 31622]
    -- sqrt 10 = 3.1622776601683795
    squares' = reverse (filter distinct squares)
    squareSet = Set.fromList squares'
    perms = [ (xs,ys) | ws <- anas, [xs,ys] <- subseqs_len 2 ws ]
    perms' = perms ++ [ (ys,xs) | (xs,ys) <- perms ]
    permute w (xs, ys)
      | length w == length xs =
          catMaybes (map (flip lookup (zip xs w)) ys)
      | otherwise = []
    ok w = any (\perm -> Set.member (permute w perm) squareSet) perms'

parseFile :: FilePath -> IO [String]
parseFile path = do
  s <- readFile path
  return (read ("[" ++ s ++ "]"))

wordsTxt :: IO [String]
wordsTxt = parseFile "words.txt"

main :: IO String
main = wordsTxt >>= return . head . prob98 . anagrams
-- "18769"
