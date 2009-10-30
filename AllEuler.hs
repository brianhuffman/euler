module Main where
import System (getArgs)
import System.Time
import Data.Char (isDigit)
import Data.List (sort)
import qualified Euler1
import qualified Euler2
import qualified Euler201
import qualified Euler202
import qualified Euler203
import qualified Euler204
import qualified Euler205
import qualified Euler206
import qualified Euler207
import qualified Euler208
import qualified Euler209
import qualified Euler210
import qualified Euler211
import qualified Euler212
import qualified Euler213
import qualified Euler214
import qualified Euler215
import qualified Euler216
import qualified Euler217
import qualified Euler218
import qualified Euler219
import qualified Euler220
import qualified Euler221
import qualified Euler222
import qualified Euler223
import qualified Euler224
import qualified Euler225
import qualified Euler226
import qualified Euler227
import qualified Euler228
import qualified Euler229
import qualified Euler230
import qualified Euler231
import qualified Euler232
import qualified Euler233
import qualified Euler234
import qualified Euler235
import qualified Euler236
import qualified Euler237
import qualified Euler238
import qualified Euler239
import qualified Euler240
import qualified Euler241
import qualified Euler242
import qualified Euler244
import qualified Euler245
import qualified Euler246
import qualified Euler247
import qualified Euler248
import qualified Euler249
import qualified Euler250
import qualified Euler251
import qualified Euler252
import qualified Euler253
import qualified Euler254
import qualified Euler255
import qualified Euler256
import qualified Euler257
import qualified Euler258
import qualified Euler259
import qualified Euler260
import qualified Euler261

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
  [
  (201, 2.0,  6, Euler201.main, "115039000", "Subsets with a unique sum"),
  (202, 0.0,  1, Euler202.main, "1209002624", "Laserbeam"),
  (203, 0.0,  3, Euler203.main, "34029210557338", "Squarefree Binomial Coefficients"),
  (204, 0.6,  1, Euler204.main, "2944730", "Generalised Hamming Numbers"),
  (205, 0.0,  1, Euler205.main, "0.5731441", "Dice Game"),
  (206, 0.1,  1, Euler206.main, "1389019170", "Concealed Square"),
  (207, 0.0,  0, Euler207.main, "44043947822", "Integer partition equations"),
  (208, 0.6,  7, Euler208.main, Euler208.answer, "Robot Walks"),
  (209, 0.0,  1, Euler209.main, "15964587728784", "Circular Logic"),
  (210,  23,  1, Euler210.main, Euler210.answer, "Obtuse Angled Triangles"),
  (211, 2.6,  2, Euler211.main, Euler211.answer, "Divisor Square Sum"),
  (212, 1.6, 20, Euler212.main, "328968937309", "Combined Volume of Cuboids"),
  (213, 6.2, 11, Euler213.main, Euler213.answer, "Flea Circus"),
  (214, 9.1, 25, Euler214.main, Euler214.answer, "Totient Chains"),
  (215, 0.6,  5, Euler215.main, "806844323190414", "Crack-free Walls"),
  (216,  84, 25, Euler216.main, "5437849", "Primality of 2n^2-1"),
  (217, 0.3,  1, Euler217.main, "6273134", "Balanced Numbers"),
  (218, 0.0,  1, Euler218.main, "0", "Perfect right-angled triangles"),
  (219, 0.0,  1, Euler219.main, "64564225042", "Skew-cost coding"),
  (220, 0.0,  1, Euler220.main, "139776,963904", "Heighway Dragon"),
  (221,  10, 47, Euler221.main, Euler221.answer, "Alexandrian Integers"),
  (222, 0.0,  1, Euler222.main, "1590933", "Sphere Packing"),
  (223, 9.4,  1, Euler223.main, Euler223.answer, "Almost right-angled triangles I"),
  (224, 0.7,  1, Euler224.main, Euler224.answer, "Almost right-angled triangles II"),
  (225, 0.2,  1, Euler225.main, Euler225.answer, "Tribonacci non-divisors"),
  (226, 0.0,  1, Euler226.main, "0.11316017", "A Scoop of Blancmange"),
  (227, 0.2,  3, Euler227.main, Euler227.answer, "The Chase"),
  (228, 0.0,  1, Euler228.main, "86226", "Minkowski Sums"),
  (229,  15, 15, Euler229.main, Euler229.answer, "Four Representations using Squares"),
  (230, 0.0,  1, Euler230.main, Euler230.answer, "Fibonacci Words"),
  (231, 1.3,  3, Euler231.main, Euler231.answer, "Prime Factorisation of Binomial Coefficients"),
  (232, 0.1,  2, Euler232.main, Euler232.answer, "The Race"),
  (233,  13, 51, Euler233.main, Euler233.answer, "Lattice Points on a Circle"),
  (234, 3.0,  7, Euler234.main, Euler234.answer, "Semidivisible Numbers"),
  (235, 0.0,  1, Euler235.main, Euler235.answer, "An Arithmetic Geometric Sequence"),
  (236,  37,  2, Euler236.main, Euler236.answer, "Luxury Hampers"),
  (237, 0.1,  1, Euler237.main, Euler237.answer, "Tours on a 4xn Playing Board"),
  -- (238, 999, Euler238.main, Euler238.answer, "Infinite String Tour"),
  (239, 0.0,  1, Euler239.main, Euler239.answer, "Twenty-two Foolish Primes"),
  (240, 0.3, 10, Euler240.main, Euler240.answer, "Top Dice"),
  -- 241  (unsolved)
  (242, 0.0,  1, Euler242.main, Euler242.answer, "Odd Triplets"),
  -- 243  (found by trial and error)
  (244, 0.6,  8, Euler244.main, Euler244.answer, "Sliders"),
  (245, 316,103, Euler245.main, Euler245.answer, "Coresilience"),
  (246, 0.2,  1, Euler246.main, Euler246.answer, "Tangents to an Ellipse"),
  (247, 1.5, 45, Euler247.main, Euler247.answer, "Squares Under a Hyperbola"),
  (248, 3.4,  3, Euler248.main, Euler248.answer, "Totient Equals 13!"),
  (249, 104, 73, Euler249.main, Euler249.answer, "Prime Subset Sums"),
  (250,  11,  2, Euler250.main, Euler250.answer, "250250"),
  (251, 547,565, Euler251.main, Euler251.answer, "Cardano Triplets"),
  (252,  35,  2, Euler252.main, Euler252.answer, "Convex Holes"),
  (253, 0.0,  1, Euler253.main, Euler253.answer, "Tidying Up"),
  (254, 8.9,  2, Euler254.main, Euler254.answer, "Sum of Digit Factorials"),
  (255, 391,  1, Euler255.main, Euler255.answer, "Rounded Square Roots"),
  (256, 1.2,  2, Euler256.main, Euler256.answer, "Tatami-Free Rooms"),
  (257,  18,  2, Euler257.main, Euler257.answer, "Angular Bisectors"),
  (258,  62,  2, Euler258.main, Euler258.answer, "A Lagged Fibonacci Sequence"),
  (259,  45,221, Euler259.main, Euler259.answer, "Reachable Numbers"),
  (260,  43, 13, Euler260.main, Euler260.answer, "Stone Game"),
  (261, 0.2,  7, Euler261.main, Euler261.answer, "Pivotal Square Sums")
  ]

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
