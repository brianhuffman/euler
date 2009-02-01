module Euler185 where
import EulerLib
import Data.Bits
import Data.Char (digitToInt)
import Data.List (sortBy)

{-
Problem 185
08 March 2008

The game Number Mind is a variant of the well known game Master Mind.

Instead of coloured pegs, you have to guess a secret sequence of
digits. After each guess you're only told in how many places you've
guessed the correct digit. So, if the sequence was 1234 and you
guessed 2036, you'd be told that you have one correct digit; however,
you would NOT be told that you also have another digit in the wrong
place.

For instance, given the following guesses for a 5-digit secret
sequence,

90342 ;2 correct
70794 ;0 correct
39458 ;2 correct
34109 ;1 correct
51545 ;2 correct
12531 ;1 correct

The correct sequence 39542 is unique.

Based on the following guesses,

5616185650518293 ;2 correct
3847439647293047 ;1 correct
5855462940810587 ;3 correct
9742855507068353 ;3 correct
4296849643607543 ;3 correct
3174248439465858 ;1 correct
4513559094146117 ;2 correct
7890971548908067 ;3 correct
8157356344118483 ;1 correct
2615250744386899 ;2 correct
8690095851526254 ;3 correct
6375711915077050 ;1 correct
6913859173121360 ;1 correct
6442889055042768 ;2 correct
2321386104303845 ;0 correct
2326509471271448 ;2 correct
5251583379644322 ;2 correct
1748270476758276 ;3 correct
4895722652190306 ;1 correct
3041631117224635 ;3 correct
1841236454324589 ;3 correct
2659862637316867 ;2 correct

Find the unique 16-digit secret sequence.
-}

type Clue = (String, Int)

test_clues :: [Clue]
test_clues =
  [("90342",2)
  ,("70794",0)
  ,("39458",2)
  ,("34109",1)
  ,("51545",2)
  ,("12531",1)
  ]

real_clues :: [Clue]
real_clues =
  [("5616185650518293",2)
  ,("3847439647293047",1)
  ,("5855462940810587",3)
  ,("9742855507068353",3)
  ,("4296849643607543",3)
  ,("3174248439465858",1)
  ,("4513559094146117",2)
  ,("7890971548908067",3)
  ,("8157356344118483",1)
  ,("2615250744386899",2)
  ,("8690095851526254",3)
  ,("6375711915077050",1)
  ,("6913859173121360",1)
  ,("6442889055042768",2)
  ,("2321386104303845",0)
  ,("2326509471271448",2)
  ,("5251583379644322",2)
  ,("1748270476758276",3)
  ,("4895722652190306",1)
  ,("3041631117224635",3)
  ,("1841236454324589",3)
  ,("2659862637316867",2)
  ]

{-
Basically, this is a search problem.
We will rely on heuristics to guide the search.

During the search, we maintain a bitmask for each position
that keeps track of which symbols could possibly fill that spot.

We consider clues one at a time. For each clue, we loop over all
possible combinations of which positions are matches.

To speed the process, we consider the clues with the least number
of matches first.

----------------------------------------
Consider the clue (s, n)
where s!x == d, length s == l

position x has
  probability n/l of being d
  probability (l-n)/9l of being any particular digit besides d

  == d -> 9*n
  /= d -> (l-n)

56161-5650518293 ;2 correct
384743964729-0-7 ;1 correct
5855462940810587 ;3 correct
97428555-7068353 ;3 correct
42968496436-75-3 ;3 correct
3174248439465-58 ;1 correct
451355909-146117 ;2 correct
78909715489-8067 ;3 correct
815735-34-118483 ;1 correct
-61525074--86-99 ;2 correct
8690095851526254 ;3 correct
6375711915077050 ;1 correct
6913859-73121360 ;1 correct
64428-9055042768 ;2 correct
2321386104303845 ;0 correct
---65094712714-8 ;2 correct
525-5-3379644322 ;2 correct
1748270476758276 ;3 correct
4895722652190306 ;1 correct
304-631-1722463- ;3 correct
184-23-45--24589 ;3 correct
-659862637-16-67 ;2 correct

-}

type Mask = Int
type Slot = Int
type Clue' = ([Slot], Int)

bit2char :: Mask -> Char
bit2char 0 = '-'
bit2char 1 = '0'
bit2char 2 = '1'
bit2char 4 = '2'
bit2char 8 = '3'
bit2char 16 = '4'
bit2char 32 = '5'
bit2char 64 = '6'
bit2char 128 = '7'
bit2char 256 = '8'
bit2char 512 = '9'
bit2char _ = '?'

number_mind :: [Clue] -> [String]
number_mind clues = map (map bit2char) (search xs0 clues'')
  where
    l :: Int
    l = length $ fst $ head clues

    clues' :: [Clue']
    clues' = map (\(s,n) -> (map digitToInt s, n)) clues

    clues'' :: [Clue']
    clues'' = sortBy (\x y -> compare (snd x) (snd y)) clues'

    xs0 :: [Mask]
    xs0 = replicate l 0x3ff

search :: [Mask] -> [Clue'] -> [[Mask]]
search xs [] = [xs]
search xs ((s,r):clues) =
  [ xs'' |
    xs' <- choices r s xs,
    xs'' <- search xs' clues ]

choices :: Int -> [Slot] -> [Mask] -> [[Mask]]
choices 0 [] [] = [[]]
choices 0 (c:cs) (x:xs)
  | x == bit c = []
  | otherwise = [ clearBit x c : bs | bs <- choices 0 cs xs ]
choices n [] [] = []
choices n (c:cs) (x:xs)
  | x == bit c = [ bit c : bs | bs <- choices (n-1) cs xs ]
  | testBit x c = [ bit c : bs | bs <- choices (n-1) cs xs ] ++
                  [ clearBit x c : bs | bs <- choices n cs xs ]
  | otherwise = [ x : bs | bs <- choices n cs xs ]

main :: IO String
main = return $ head $ number_mind real_clues

answer :: String
answer = "4640261571849533"
