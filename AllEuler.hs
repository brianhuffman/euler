module Main where
import System (getArgs)
import System.Time
import Data.Char (isDigit)
import Data.List (sort)
import qualified Euler1
import qualified Euler2
import qualified Euler3
import qualified Euler4

timeIO_ :: IO () -> IO TimeDiff
timeIO_ action =
  do t1 <- getClockTime
     action
     t2 <- getClockTime
     return (diffClockTimes t2 t1)

timeIO :: IO a -> IO (a, TimeDiff)
timeIO action =
  do t1 <- getClockTime
     x <- action
     t2 <- getClockTime
     return (x, diffClockTimes t2 t1)

check :: Int -> IO String -> String -> String -> IO ()
check n action answer descr =
  do putStrLn (show n ++ ": " ++ descr)
     (x, t) <- timeIO (printIO action)
     if x /= answer
       then putStrLn ("\tIncorrect! Should be " ++ answer)
       else return ()
     let s = tdSec t
     if s > 0
       then putStrLn ("\t(" ++ show s ++ " s)")
       else return ()
  where
    printIO action = do
      x <- action
      putStrLn ('\t' : x)
      return x
--check _ _ _ = return ()


test :: Int -> IO () -> IO ()
test n@0 action =
  do putStr (show n ++ ": ")
     t <- timeIO_ action
     let s = tdSec t
     if s > 0
       then putStrLn ("\t(" ++ show s ++ " s)")
       else return ()
test _ _ = return ()

{-
Productivity:
240: (18%) Top Dice
221: (20%) Alexandrian Integers
149: (27%) Searching for a maximum-sum subsequence
201: (32%) Subsets with a unique sum
181: (40%) Grouping objects of two different colours.
161: (47%) Triominoes
160: (53%) Factorial trailing digits
143: (54%) Torricelli point of a triangle
254: (62%) Sum of Digit Factorials
259: (62%) Reachable Numbers
195: (63%) Inscribed circles
189: (63%) Tri-colouring a triangular grid
247: (65%) Squares Under a Hyperbola
159: (70%) Digital root sums of factorizations
212: (70%) Combined Volume of Cuboids
244: (70%) Sliders
126: (75%) Cubes Required to Cover a Cuboid
141: (83%) Progressive perfect squares
155: (85%) Counting Capacitor Circuits

-}

parseArg :: String -> Int -> Bool
parseArg s n
  | s == "-" = True
  | all isDigit s = n == read s
  | head s == '-' && all isDigit (tail s) = n <= read (tail s)
  | head (dropWhile isDigit s) == '-' =
      read (takeWhile isDigit s) <= n && parseArg (dropWhile isDigit s) n
  | otherwise = False

parseArgs :: [String] -> (Int -> Double -> Bool)
parseArgs (('l':w):ws) n l = l <= read w && parseArgs ws n l
parseArgs (('g':w):ws) n l = l >= read w && parseArgs ws n l
parseArgs [] _ _ = True
parseArgs xs n l = or [ parseArg x n | x <- xs ]

-- it takes about 30s to run problems 1-100 (12 Nov 2008)

-- it takes about 17 sec to run problems 1-100 (22 Oct 2009)
-- it takes about 1 min to run problems 101-150 (22 Oct 2009)
-- it takes about 4 min to run problems 151-200 (22 Oct 2009)


checks :: [(Int, Double, Int, IO String, String, String)]
checks =
  Euler1.checks ++
  Euler2.checks ++
  Euler3.checks ++
  Euler4.checks

total_time :: Double
total_time = sum [ t | (_, t, _, _, _, _) <- checks ]

most_time :: IO ()
most_time = mapM_ go (take 30 (reverse ts))
  where ts = sort [ (l, n, d) | (n, l, _, _, _, d) <- checks ]
        go (l, n, d) = putStrLn (show n ++ ": (" ++ show l ++ " s) " ++ d)

most_space :: IO ()
most_space = mapM_ go (take 30 (reverse ts))
  where ts = sort [ (s, n, d) | (n, _, s, _, _, d) <- checks ]
        go (s, n, d) = putStrLn (show n ++ ": (" ++ show s ++ " MB) " ++ d)

main :: IO ()
main = do
  args <- System.getArgs
  let p = parseArgs args
  sequence_ [ check n m s d | (n, l, _, m, s, d) <- checks, p n l ]
