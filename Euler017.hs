module Euler017 where
import Char

------------------------------------------------------------------------------
-- 17. How many letters would be needed to write all the numbers in words from 1 to 1000?

spell n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | n == 10 = "ten"
  | n == 11 = "eleven"
  | n == 12 = "twelve"
  | n == 13 = "thirteen"
  | n == 14 = "fourteen"
  | n == 15 = "fifteen"
  | n == 16 = "sixteen"
  | n == 17 = "seventeen"
  | n == 18 = "eighteen"
  | n == 19 = "nineteen"
  | n == 20 = "twenty"
  | n == 30 = "thirty"
  | n == 40 = "forty"
  | n == 50 = "fifty"
  | n == 60 = "sixty"
  | n == 70 = "seventy"
  | n == 80 = "eighty"
  | n == 90 = "ninety"
  | n < 100 = let r = mod n 10 in concat [spell (n-r), "-", spell r]
  | n < 1000 =
      let (q,r) = divMod n 100 in unwords $
        [spell q, "hundred"] ++ if r == 0 then [] else ["and", spell r]
  | n == 1000 = "one thousand"
  | otherwise = "a lot"

prob17 :: Int -> Int
prob17 n = length $ filter isAlpha $ concatMap spell [1 .. n]

main :: IO String
main = return $ show $ prob17 1000
-- 21124
