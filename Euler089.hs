module Euler089 where

------------------------------------------------------------------------------
-- 89. Develop a method to express Roman numerals in minimal form.

romanTxt :: IO [String]
romanTxt = readFile "roman.txt" >>= return . lines . filter (/='\r')

minimal_roman :: String -> String
minimal_roman s = case s of
  'M':xs                 -> 'M':minimal_roman xs
  'D':'D':xs             -> 'M':minimal_roman xs
  'D':'C':'C':'C':'C':xs -> 'C':'M':minimal_roman xs
  'D':xs                 -> 'D':minimal_roman xs
  'C':'C':'C':'C':xs     -> 'C':'D':minimal_roman xs
  'C':xs                 -> 'C':minimal_roman xs
  'L':'L':xs             -> 'C':minimal_roman xs
  'L':'X':'X':'X':'X':xs -> 'X':'C':minimal_roman xs
  'L':xs                 -> 'L':minimal_roman xs
  'X':'X':'X':'X':xs     -> 'X':'L':minimal_roman xs
  'X':xs                 -> 'X':minimal_roman xs
  'V':'V':xs             -> 'X':minimal_roman xs
  'V':'I':'I':'I':'I':xs -> 'I':'X':minimal_roman xs
  'V':xs                 -> 'V':minimal_roman xs
  'I':'I':'I':'I':xs     -> 'I':'V':minimal_roman xs
  'I':xs                 -> 'I':minimal_roman xs
  []                     -> []

is_minimal_roman :: String -> Bool
is_minimal_roman s = f1 s
  where
    f1 ('M':xs) = f1 xs
    f1 xs = f2 xs
    f2 ('C':'M':xs) = f3 xs
    f2 ('D':'C':'C':'C':xs) = f3 xs
    f2 ('D':'C':'C':xs) = f3 xs
    f2 ('D':'C':xs) = f3 xs
    f2 ('D':xs) = f3 xs
    f2 ('C':'D':xs) = f3 xs
    f2 ('C':'C':'C':xs) = f3 xs
    f2 ('C':'C':xs) = f3 xs
    f2 ('C':xs) = f3 xs
    f2 xs = f3 xs
    f3 ('X':'C':xs) = f4 xs
    f3 ('L':'X':'X':'X':xs) = f4 xs
    f3 ('L':'X':'X':xs) = f4 xs
    f3 ('L':'X':xs) = f4 xs
    f3 ('L':xs) = f4 xs
    f3 ('X':'L':xs) = f4 xs
    f3 ('X':'X':'X':xs) = f4 xs
    f3 ('X':'X':xs) = f4 xs
    f3 ('X':xs) = f4 xs
    f3 xs = f4 xs
    f4 xs = xs `elem` ["","I","II","III","IV","V","VI","VII","VIII","IX"]

roman_savings :: [String] -> Int
roman_savings xs =
  sum (map length xs) -
  sum (map (length . minimal_roman) xs)

main :: IO String
main = romanTxt >>= return . show . roman_savings
--743

