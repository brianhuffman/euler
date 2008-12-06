module Euler220 where

{-
Problem 220
06 December 2008

Let D_(0) be the two-letter string "Fa". For n≥1, derive D_(n) from
D_(n-1) by the string-rewriting rules:

"a" → "aRbFR"
"b" → "LFaLb"

Thus, D_(0) = "Fa", D_(1) = "FaRbFR", D_(2) = "FaRbFRRLFaLbFR", and so on.

These strings can be interpreted as instructions to a computer
graphics program, with "F" meaning "draw forward one unit", "L"
meaning "turn left 90 degrees", "R" meaning "turn right 90 degrees",
and "a" and "b" being ignored. The initial position of the computer
cursor is (0,0), pointing up towards (0,1).

Then D_(n) is an exotic drawing known as the Heighway Dragon of order
n. For example, D_(10) is shown below; counting each "F" as one step,
the highlighted spot at (18,16) is the position reached after 500
steps.

What is the position of the cursor after 10^(12) steps in D_(50) ?
Give your answer in the form x,y with no spaces.
-}

a :: Int -> String
a 0 = "a"
a n = a (n-1) ++ "R" ++ b (n-1) ++ "FR"

b :: Int -> String
b 0 = "b"
b n = "LF" ++ a (n-1) ++ "L" ++ b (n-1)

d :: Int -> String
d n = "F" ++ a n

run :: String -> ((Int, Int), Int)
run [] = ((0, 0), 0)
run (c : cs) =
  case c of
    'F' -> ((x, y+1), n+1)
    'L' -> ((-y, x), n)
    'R' -> ((y, -x), n)
    _ -> ((x, y), n)
  where
    ((x, y), n) = run cs

------------------------------------------

type Z = Integer

a_seq :: [(Z, Z)]
a_seq = f 0 0
  where f x y = (x, y) : f (x+y+1) (y-x)

a_dist :: Int -> Integer
a_dist n = 2^n - 1

data Cmd = L | R | F | A Int | B Int

data Dir = North | West | South | East
  deriving Show

type Move = (Integer, Integer, Dir)

thenDir :: Dir -> Dir -> Dir
thenDir North y = y
thenDir x North = x
thenDir West West = South
thenDir West South = East
thenDir West East = North
thenDir South West = East
thenDir South South = North
thenDir South East = West
thenDir East West = North
thenDir East South = West
thenDir East East = South

thenMove :: Move -> Move -> Move
thenMove (x1, y1, North) (x2, y2, d) = (x1+x2, y1+y2, d)
thenMove (x1, y1, South) (x2, y2, d) = (x1-x2, y1-y2, thenDir South d)
thenMove (x1, y1, East) (x2, y2, d) = (x1+y2, y1-x2, thenDir East d)
thenMove (x1, y1, West) (x2, y2, d) = (x1-y2, y1+x2, thenDir West d)

runCmd :: Cmd -> Integer -> (Move, Maybe Integer)
runCmd L m = ((0, 0, West), Just m)
runCmd R m = ((0, 0, East), Just m)
runCmd F m
  | m < 1 = ((0, 0, North), Nothing)
  | otherwise = ((0, 1, North), Just (m-1))
runCmd (A 0) m = ((0, 0, North), Just m)
runCmd (A n) m
  | m < a_dist n = runCmds [A(n-1), R, B(n-1), F, R] m
  | otherwise = let (x, y) = a_seq!!n in ((x, y, South), Just (m - a_dist n))
runCmd (B 0) m = ((0, 0, North), Just m)
runCmd (B n) m
  | m < a_dist n = runCmds [L, F, A(n-1), L, B(n-1)] m
  | otherwise = let (x, y) = a_seq!!n in ((-x, -y, South), Just (m - a_dist n))

runCmds :: [Cmd] -> Integer -> (Move, Maybe Integer)
runCmds [] m = ((0, 0, North), Just m)
runCmds (c : cs) m =
  let (move1, mm') = runCmd c m in
    case mm' of
      Nothing -> (move1, Nothing)
      Just m' -> let (move2, mm'') = runCmds cs m' in
                   (move1 `thenMove` move2, mm'')

main :: IO String
main = return (show x ++ "," ++ show y)
  where
    ((x,y,d),_) = runCmds [F, A 50] (10^12)
