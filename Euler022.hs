module Euler022 where
import Data.Char
import Data.List

parseFile :: FilePath -> IO [String]
parseFile path = do
  s <- readFile path
  return (read ("[" ++ s ++ "]"))

namesTxt :: IO [String]
namesTxt = parseFile "names.txt"

------------------------------------------------------------------------------
-- 22. What is the total of all the name scores in the file of first names?

word_value :: String -> Int
word_value = sum . map letter_value
  where letter_value = subtract 64 . ord

prob22 :: [String] -> Int
prob22 = sum . zipWith (\n name -> n * word_value name) [1..] . sort

main :: IO String
main = namesTxt >>= return . show . prob22
-- 871198282

