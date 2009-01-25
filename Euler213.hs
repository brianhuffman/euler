module Euler213 where
import Data.Array.Unboxed
import System.Random
import Data.List

{-

Problem 213
18 October 2008

A 30×30 grid of squares contains 900 fleas, initially one flea per
square.  When a bell is rung, each flea jumps to an adjacent square at
random (usually 4 possibilities, except for fleas on the edge of the
grid or at the corners).

What is the expected number of unoccupied squares after 50 rings of
the bell? Give your answer rounded to six decimal places.

-}

{-
Analysis:
The expected number of empty squares is equal to
SUM (i,j):squares. P(square (i,j) is empty)

-}

type Prob = Double
type Pos = (Int, Int)
type Dist = UArray Pos Prob

nextDist :: Dist -> Dist
nextDist dist = accumArray (+) 0 (bounds dist) (concatMap p (indices dist))
  where
    ((x0,y0),(x1,y1)) = bounds dist
    p (x, y)
      | x == x0 && y == y0 = choose 2 [(x+1, y), (x, y+1)]
      | x == x0 && y == y1 = choose 2 [(x+1, y), (x, y-1)]
      | x == x1 && y == y0 = choose 2 [(x-1, y), (x, y+1)]
      | x == x1 && y == y1 = choose 2 [(x-1, y), (x, y-1)]
      | x == x0 = choose 3 [(x+1, y), (x, y-1), (x, y+1)]
      | x == x1 = choose 3 [(x-1, y), (x, y-1), (x, y+1)]
      | y == y0 = choose 3 [(x, y+1), (x-1, y), (x+1, y)]
      | y == y1 = choose 3 [(x, y-1), (x-1, y), (x+1, y)]
      | otherwise = choose 4 [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
     where choose n = map (\xy -> (xy, dist!(x, y) / n))

initDist :: (Pos, Pos) -> Pos -> Dist
initDist ij a = accumArray (const id) 0 ij [(a, 1)]

-------------------------------------------------------

positions :: Int -> [Pos]
positions n = [ (i, j) | i <- [1 .. n], j <- [1 .. n] ]

finalDists :: Int -> Int -> [Dist]
finalDists n k =
  [ iterate nextDist (initDist bnds pos) !! k | pos <- positions n ]
  where bnds = ((1, 1), (n, n))

prob213 :: Int -> Int -> [(Pos, Prob)]
prob213 n k = [ (p, empty p) | p <- positions n ]
  where
    dists = finalDists n k
    empty p = product [ 1 - d!p | d <- dists ]

show_rounded :: Int -> Double -> String
show_rounded d x =
  show (floor x) ++ "." ++ replicate (d - length (show r)) '0' ++ show r
  where
    r = round (x * 10^d) `mod` (10^d)

main :: IO String
main = return $ show_rounded 6 $ sum $ map snd $ prob213 30 50

answer :: String
answer = "330.721154"

-------------------------------------------------------
-- Simulation

type Range = (Int, Int)

choose2 :: a -> a -> IO a
choose2 a b = do
  x <- randomIO
  return (if x then a else b)

choose3 :: a -> a -> a -> IO a
choose3 a b c = do
  x <- randomRIO (0,2)
  case (x::Int) of
    0 -> return a
    1 -> return b
    2 -> return c

choose4 :: a -> a -> a -> a -> IO a
choose4 a b c d = do
  x <- randomRIO (0,3)
  case (x::Int) of
    0 -> return a
    1 -> return b
    2 -> return c
    3 -> return d

nextPos :: Range -> Pos -> IO Pos
nextPos (i,j) (x,y)
  | x == i && y == i = choose2 (x+1, y) (x, y+1)
  | x == i && y == j = choose2 (x+1, y) (x, y-1)
  | x == j && y == i = choose2 (x-1, y) (x, y+1)
  | x == j && y == j = choose2 (x-1, y) (x, y-1)
  | x == i = choose3 (x+1, y) (x, y-1) (x, y+1)
  | x == j = choose3 (x-1, y) (x, y-1) (x, y+1)
  | y == i = choose3 (x, y+1) (x-1, y) (x+1, y)
  | y == j = choose3 (x, y-1) (x-1, y) (x+1, y)
  | otherwise = choose4 (x+1, y) (x-1, y) (x, y+1) (x, y-1)

nextAll :: Range -> [Pos] -> IO [Pos]
nextAll r = mapM (nextPos r)

manyAll :: Range -> Int -> [Pos] -> IO [Pos]
manyAll r 0 ps = return ps
manyAll r n ps = do
  ps' <- nextAll r ps
  manyAll r (n - 1) ps'

distincts :: [Pos] -> Int
distincts ps = len (sort ps)
  where
    len (x:xs@(y:zs)) = if x == y then len xs else 1 + len xs
    len [x] = 1
    len [] = 0

r30 :: Range
r30 = (1, 30)

ps30 :: [Pos]
ps30 = [ (i, j) | i <- [1 .. 30], j <- [1 .. 30] ]

ps30' :: [Pos]
ps30' = [ (i, j) | i <- [1 .. 30], j <- [1 .. 30], even (i + j) ]

test50 :: IO [Pos]
test50 = manyAll r30 50 ps30'

t50 :: IO Int
t50 = do
  n <- test50
  return (900 - 2 * (distincts n))

test :: IO ()
test = run 0 0
  where
    run :: Double -> Double -> IO ()
    run n x = do
      ks <- sequence (replicate 100 t50)
      let x' = x + fromIntegral (sum ks)
      let n' = n + 100
      print (n', x'/n')
      run n' x'

{-
Based on experiments, average seems to be
about 330.7 empty squares.
-}