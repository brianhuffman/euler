module Euler054 where
import EulerLib
import List

pokerTxt :: IO [([String],[String])]
pokerTxt = readFile "poker.txt" >>= return .
  map (splitAt 5 . words) . lines

------------------------------------------------------------------------------
-- 54. How many hands did player one win in the game of poker?

parseCard s = (rank (reverse (drop 1 (reverse s))), last s)
  where
    rank "T" = 10
    rank "J" = 11
    rank "Q" = 12
    rank "K" = 13
    rank "A" = 14
    rank r = read r

pokerHand ws
  | straight && flush = (9, repeats)
  | flush             = (6, repeats)
  | straight          = (5, repeats)
  | otherwise = case counts of
      [4,1] -> (8, repeats)
      [3,2] -> (7, repeats)
      [3,1,1] -> (4, repeats)
      [2,2,1] -> (3, repeats)
      [2,1,1,1] -> (2, repeats)
      [1,1,1,1,1] -> (1, repeats)
  where
    cards = sort (map parseCard ws)
    ranks = map fst cards
    suits = map snd cards
    flush = all_same suits
    straight = ranks == [head ranks .. head ranks + 4]
    lengths = map (\ys -> (length ys, head ys))
    repeats = reverse $ sort $ lengths $ group $ ranks
    counts = map fst repeats

main :: IO String
main = pokerTxt >>= return . show .
  length . filter (\(h1,h2) -> pokerHand h1 > pokerHand h2)
-- 376
