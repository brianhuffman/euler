module Euler042 where
import qualified SortedList as S
import Data.List (sort)
import Data.Char (ord)

------------------------------------------------------------------------------
-- 42. How many triangle words does the list of common English words contain?

triangle_seq :: [Int]
triangle_seq = scanl1 (+) [0 ..]

word_value :: String -> Int
word_value = sum . map letter_value
  where letter_value = subtract 64 . ord

parseFile :: FilePath -> IO [String]
parseFile path = do
  s <- readFile path
  return (read ("[" ++ s ++ "]"))

wordsTxt :: IO [String]
wordsTxt = parseFile "words.txt"

prob42 :: [String] -> Int
prob42 ws = length $ S.intersect scores triangle_seq
  where scores = sort (map word_value ws)

main :: IO String
main = wordsTxt >>= return . show . prob42
-- 162

