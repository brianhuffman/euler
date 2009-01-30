module Main where
import System (getArgs)
import System.Time
import Char (isDigit)
import qualified Euler001
import qualified Euler002
import qualified Euler003
import qualified Euler004
import qualified Euler005
import qualified Euler006
import qualified Euler007
import qualified Euler008
import qualified Euler009
import qualified Euler010
import qualified Euler011
import qualified Euler012
import qualified Euler013
import qualified Euler014
import qualified Euler015
import qualified Euler016
import qualified Euler017
import qualified Euler018
import qualified Euler019
import qualified Euler020
import qualified Euler021
import qualified Euler022
import qualified Euler023
import qualified Euler024
import qualified Euler025
import qualified Euler026
import qualified Euler027
import qualified Euler028
import qualified Euler029
import qualified Euler030
import qualified Euler031
import qualified Euler032
import qualified Euler033
import qualified Euler034
import qualified Euler035
import qualified Euler036
import qualified Euler037
import qualified Euler038
import qualified Euler039
import qualified Euler040
import qualified Euler041
import qualified Euler042
import qualified Euler043
import qualified Euler044
import qualified Euler045
import qualified Euler046
import qualified Euler047
import qualified Euler048
import qualified Euler049
import qualified Euler050
import qualified Euler051
import qualified Euler052
import qualified Euler053
import qualified Euler054
import qualified Euler055
import qualified Euler056
import qualified Euler057
import qualified Euler058
import qualified Euler059
import qualified Euler060
import qualified Euler061
import qualified Euler062
import qualified Euler063
import qualified Euler064
import qualified Euler065
import qualified Euler066
import qualified Euler067
import qualified Euler068
import qualified Euler069
import qualified Euler070
import qualified Euler071
import qualified Euler072
import qualified Euler073
import qualified Euler074
import qualified Euler075
import qualified Euler076
import qualified Euler077
import qualified Euler078
import qualified Euler079
import qualified Euler080
import qualified Euler081
import qualified Euler082
import qualified Euler083
import qualified Euler084
import qualified Euler085
import qualified Euler086
import qualified Euler087
import qualified Euler088
import qualified Euler089
import qualified Euler090
import qualified Euler091
import qualified Euler092
import qualified Euler093
import qualified Euler094
import qualified Euler095
import qualified Euler096
import qualified Euler097
import qualified Euler098
import qualified Euler099
import qualified Euler100
import qualified Euler101
import qualified Euler102
import qualified Euler103
import qualified Euler104
import qualified Euler105
import qualified Euler106
import qualified Euler107
import qualified Euler108
import qualified Euler109
import qualified Euler110
import qualified Euler111
import qualified Euler112
import qualified Euler113
import qualified Euler114
import qualified Euler115
import qualified Euler116
import qualified Euler117
import qualified Euler118
import qualified Euler119
import qualified Euler120
import qualified Euler121
import qualified Euler122
import qualified Euler123
import qualified Euler124
import qualified Euler125
import qualified Euler126
import qualified Euler127
import qualified Euler128
import qualified Euler129
import qualified Euler130
import qualified Euler131
import qualified Euler132
import qualified Euler133
import qualified Euler134
import qualified Euler135
import qualified Euler136
import qualified Euler137
import qualified Euler138
import qualified Euler139
import qualified Euler140
import qualified Euler141
import qualified Euler142
import qualified Euler143
import qualified Euler144
import qualified Euler145
import qualified Euler146
import qualified Euler147
import qualified Euler148
import qualified Euler149
import qualified Euler150
import qualified Euler151
import qualified Euler152
import qualified Euler153
import qualified Euler154
import qualified Euler155
import qualified Euler156
import qualified Euler157
import qualified Euler158
import qualified Euler159
import qualified Euler160
import qualified Euler161
import qualified Euler162
import qualified Euler163
import qualified Euler164
import qualified Euler165
import qualified Euler166
import qualified Euler167
import qualified Euler168
import qualified Euler169
import qualified Euler170
import qualified Euler171
import qualified Euler172
import qualified Euler173
import qualified Euler174
import qualified Euler175
import qualified Euler176
import qualified Euler177
import qualified Euler178
import qualified Euler179
import qualified Euler180
import qualified Euler181
import qualified Euler182
import qualified Euler183
import qualified Euler184
import qualified Euler185
import qualified Euler186
import qualified Euler187
import qualified Euler188
import qualified Euler189
import qualified Euler190
import qualified Euler191
import qualified Euler192
import qualified Euler193
import qualified Euler194
import qualified Euler195
import qualified Euler196
import qualified Euler197
import qualified Euler198
import qualified Euler199
import qualified Euler200
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
Slow:
165: (300 s) Intersections
154: (80 s) Exploring Pascal's pyramid
196: (62 s) Prime triplets
216: (61 s) Primality of 2n^2-1
210: (50 s) Obtuse Angled Triangles
153: (33 s) Investigating Gaussian Integers
150: (32 s) Minimum-sum sub-triangles
155: (14 s) Counting Capacitor Circuits
229: (12 s) Four Representations using Squares
193: (11 s) Squarefree Numbers
141: (10 s) Progressive perfect squares
185: (9 s) Number Mind

Productivity:
149: (17%) Searching for a maximum-sum subsequence
208: (21%) Robot Walks
221: (23%) Alexandrian Integers
201: (29%) Subsets with a unique sum
161: (40%) Triominoes
181: (40%) Grouping objects of two different colours.
155: (46%) Counting Capacitor Circuits
143: (48%) Torricelli point of a triangle
160: (52%) Factorial trailing digits
195: (56%) Inscribed circles
189: (60%) Tri-colouring a triangular grid
126: (66%)
159: (66%) Digital root sums of factorizations
227: (70%) The Chase
212: (73%) Combined Volume of Cuboids
141: (77%) Progressive perfect squares

Memory:
149: (175 Mb) Searching for a maximum-sum subsequence
155: (156 Mb) Counting Capacitor Circuits
179: (79 Mb) Consecutive positive divisors
187: (76 Mb) Semiprimes
195: (71 Mb) Inscribed circles
181: (61 Mb) Grouping objects of two different colours
159: (60 Mb) Digital root sums of factorizations
221: (56 Mb) Alexandrian Integers
161: (44 Mb) Triominoes
193: (26 Mb) Squarefree Numbers
214: (25 Mb) Totient Chains
216: (23 Mb) Primality of 2n^2-1
141: (22 Mb) Progressive perfect squares
189: (21 Mb) Tri-colouring a triangular grid
212: (19 Mb) Combined Volume of Cuboids
124: (18 Mb)
75: (17 Mb)
88: (16 Mb)
143: (16 Mb) Torricelli point of a triangle
196: (15 Mb) Prime triplets
229: (15 Mb) Four Representations using Squares
127: (14 Mb)
213: (10 Mb) Flea Circus



-}

parseArg :: String -> Int -> Bool
parseArg s n
  | s == "-" = True
  | all isDigit s = n == read s
  | head s == '-' && all isDigit (tail s) = n <= read (tail s)
  | head (dropWhile isDigit s) == '-' =
      read (takeWhile isDigit s) <= n && parseArg (dropWhile isDigit s) n
  | otherwise = False

parseArgs :: [String] -> (Int -> Int -> Bool)
parseArgs (('l':w):ws) n l = l <= read w && parseArgs ws n l
parseArgs (('g':w):ws) n l = l >= read w && parseArgs ws n l
parseArgs [] _ _ = True
parseArgs xs n l = or [ parseArg x n | x <- xs ]

-- it takes about 30s to run problems 1-100 (12 Nov 2008)

checks :: [(Int, Int, IO String, String, String)]
checks = [
  (  1,   0, Euler001.main, "233168", "Add multiples of 3 or 5"),
  (  2,   0, Euler002.main, "4613732", "Sum of even-valued Fibonacci numbers"),
  (  3,   0, Euler003.main, "6857", "Find the largest prime factor"),
  (  4,   0, Euler004.main, "906609", "Largest palindrome made from a product"),
  (  5,   0, Euler005.main, "232792560", "Smallest number divisible by 1 to 20"),
  (  6,   0, Euler006.main, "25164150", "Sum of squares - square of sums"),
  (  7,   1, Euler007.main, "104743", "Find the 10001st prime"),
  (  8,   0, Euler008.main, "40824", "Largest product of 5 consecutive digits"),
  (  9,   0, Euler009.main, "31875000", "Pythagorean triplet with a+b+c=1000"),
  ( 10,   1, Euler010.main, "142913828922", "Sum of primes below 2 million"),
  ( 11,   0, Euler011.main, "70600674", "Greatest product of 4 numbers"),
  ( 12,   2, Euler012.main, "76576500", "Rriangle number with 500 divisors"),
  ( 13,   0, Euler013.main, "5537376230", "Sum of one-hundred 50-digit numbers"),
  ( 14,  12, Euler014.main, "837799", "Longest sequence starting under a million"),
  ( 15,   0, Euler015.main, "137846528820", "How many routes to the bottom corner"),
  ( 16,   0, Euler016.main, "1366", "Sum of the digits of 2^1000"),
  ( 17,   0, Euler017.main, "21124", "How many letters to write 1 to 1000"),
  ( 18,   0, Euler018.main, "1074", "Maximum sum travelling down the triangle"),
  ( 19,   0, Euler019.main, "171", "Sundays on the first of the month"),
  ( 20,   0, Euler020.main, "648", "Sum of the digits in 100 factorial"),
  ( 21,   2, Euler021.main, "31626", "Sum of amicable pairs under 10000"),
  ( 22,   2, Euler022.main, "871198282", "Total of all name scores in file"),
  ( 23,   8, Euler023.main, "4179871", "Sums of two abundant numbers"),
  ( 24,   0, Euler024.main, "2783915460", "Millionth lexicographic permutation"),
  ( 25,   0, Euler025.main, "4782", "First Fibonacci number with 1000 digits"),
  ( 26,   0, Euler026.main, "983", "1/d containing the longest cycle"),
  ( 27,   4, Euler027.main, "-59231", "Quadratic formula that produces primes"),
  ( 28,   0, Euler028.main, "669171001", "Sum of diagonals in a spiral"),
  ( 29,   1, Euler029.main, "9183", "Distinct terms generated by a^b"),
  ( 30,   1, Euler030.main, "443839", "Sums of fifth powers of digits"),
  ( 31,   0, Euler031.main, "73682", "Combinations of currency denominations"),
  ( 32,   1, Euler032.main, "45228", "Sum of pandigital products"),
  ( 33,   0, Euler033.main, "100", "Fractions with unorthodox cancelling"),
  ( 34,   1, Euler034.main, "40730", "Sums of factorials of digits"),
  ( 35,   2, Euler035.main, "55", "Circular primes"),
  ( 36,   0, Euler036.main, "872187", "Palindromes in base 10 and base 2"),
  ( 37,   5, Euler037.main, "748317", "Truncatable primes"),
  ( 38,   0, Euler038.main, "932718654", "Largest pandigital"),
  ( 39,   0, Euler039.main, "840", "Perimeter with most right triangles"),
  ( 40,   0, Euler040.main, Euler040.answer, "Finding the nth digit"),
  ( 41,   0, Euler041.main, "7652413", "Largest pandigital prime"),
  ( 42,   1, Euler042.main, "162", "Triangle words"),
  ( 43,   0, Euler043.main, "16695334890", "Sub-string divisibility"),
  ( 44,   1, Euler044.main, "5482660", "Pentagonal sum and difference"),
  ( 45,   1, Euler045.main, "1533776805", "Pentagonal, hexagonal triangle number"),
  ( 46,   0, Euler046.main, "5777", "Sum of a prime and twice a square"),
  ( 47,   3, Euler047.main, "134043", "Consecutive integers with 4 prime factors"),
  ( 48,   0, Euler048.main, "9110846700", "1^1 + 2^2 + ... + 1000^1000"),
  ( 49,   3, Euler049.main, "296962999629", "Prime permutation arithmetic sequences"),
  ( 50,   0, Euler050.main, "997651", "Prime as the sum of consecutive primes"),
  ( 51,   4, Euler051.main, "121313", "Changing a part to form 8 primes"),
  ( 52,   0, Euler052.main, "142857", "2x, 3x, 4x, 5x, 6x with the same digits"),
  ( 53,   0, Euler053.main, "4075", "C(n,r) exceeding 1 million"),
  ( 54,   1, Euler054.main, "376", "Poker Hands"),
  ( 55,   4, Euler055.main, "249", "Lychrel Numbers"),
  ( 56,   2, Euler056.main, "972", "Maximum digital sum of a^b"),
  ( 57,   1, Euler057.main, "153", "Continued fraction for square root of 2"),
  ( 58,   9, Euler058.main, "26241", "Primes on diagonals of spiral grid"),
  ( 59,   0, Euler059.main, "107359" , "Decrypt XOR cipher"),
  ( 60,  46, Euler060.main, Euler060.answer, "Concatenating a set of five primes"),
  ( 61,   0, Euler061.main, "28684", "Figurate numbers with a cyclic property"),
  ( 62,   3, Euler062.main, "127035954683", "Digit permutations are cubes"),
  ( 63,   0, Euler063.main, "49", "n-digit integers which are nth powers"),
  ( 64,   3, Euler064.main, "1322", "Continued fractions with odd period"),
  ( 65,   0, Euler065.main, "272", "Convergents of continued fraction for e"),
  ( 66,   1, Euler066.main, "661", "Diophantine equation x^2 - Dy^2 = 1"),
  ( 67,   1, Euler067.main, "7273", "Maximal sum in the triangle"),
  ( 68,   0, Euler068.main, "6531031914842725", "Magic 5-gon ring"),
  ( 69,   0, Euler069.main, "510510", "Maximize n / phi(n)"),
  ( 70,   2, Euler070.main, "8319823", "phi(n) as a permutation of n"),
  ( 71,   0, Euler071.main, "428570", "Listing proper fractions in order"),
  ( 72,   5, Euler072.main, "303963552391", "Counting proper fractions"),
  ( 73,   2, Euler073.main, "5066251", "Proper fractions between 1/3 and 1/2"),
  ( 74,   9, Euler074.main, "402", "Factorial Chains"),
  ( 75,   9, Euler075.main, "214954", "Lengths forming a unique right triangle"),
  ( 76,   0, Euler076.main, "190569291", "100 as sum of at least two integers"),
  ( 77,   0, Euler077.main, "71", "Value as sum of primes in 5000 ways"),
  ( 78,   9, Euler078.main, "55374", "Separating coins into piles"),
  ( 79,   0, Euler079.main, "73162890", "Determine the secret passcode"),
  ( 80,   1, Euler080.main, "40886", "Sum of digits of square roots"),
  ( 81,   1, Euler081.main, "427337", ""),
  ( 82,   1, Euler082.main, "260324", ""),
  ( 83,   8, Euler083.main, "425185", ""),
  ( 84,   0, Euler084.main, "101524", ""),
  ( 85,   0, Euler085.main, "2772", ""),
  ( 86,   0, Euler086.main, "1818", ""),
  ( 87,   9, Euler087.main, "1097343", ""),
  ( 88,   9, Euler088.main, "7587457", ""),
  ( 89,   0, Euler089.main, "743", ""),
  ( 90,   0, Euler090.main, "1217", ""),
  ( 91,   3, Euler091.main, "14234", ""),
  ( 92,   0, Euler092.main, "8581146", ""),
  ( 93,   8, Euler093.main, "1258", ""),
  ( 94,   0, Euler094.main, "518408346", ""),
  ( 95,   7, Euler095.main, Euler095.answer, "The longest amicable chain"),
  ( 96,  15, Euler096.main, "24702", "Solving Su Doku puzzles"),
  ( 97,   0, Euler097.main, "8739992577", ""),
  ( 98,   1, Euler098.main, "18769", ""),
  ( 99,   1, Euler099.main, "709", ""),
  (100,   0, Euler100.main, "756872327473", ""),
  (101,   0, Euler101.main, "37076114526", ""),
  (102,   1, Euler102.main, "228", ""),
  (103,   6, Euler103.main, "20313839404245", ""),
  (104,   7, Euler104.main, "329468", ""),
  (105,   2, Euler105.main, "73702", ""),
  (106,   0, Euler106.main, "21384", ""),
  (107,   0, Euler107.main, "259679", ""),
  (108,   0, Euler108.main, "180180", ""),
  (109,   0, Euler109.main, "38182", ""),
  (110,   4, Euler110.main, "9350130049860600", ""),
  (111,   6, Euler111.main, "612407567715", ""),
  (112,   7, Euler112.main, "1587000", ""),
  (113,   0, Euler113.main, "51161058134250", ""),
  (114,   0, Euler114.main, "16475640049", ""),
  (115,   0, Euler115.main, "168", ""),
  (116,   0, Euler116.main, "20492570929", ""),
  (117,   0, Euler117.main, "100808458960497", ""),
  (118,  43, Euler118.main, "44680", ""),
  (119,   0, Euler119.main, "248155780267521", ""),
  (120,   0, Euler120.main, "333082500", ""),
  (121,   0, Euler121.main, "2269", ""),
  (122,  17, Euler122.main, "1582", ""),
  (123,   2, Euler123.main, "21035", ""),
  (124,  15, Euler124.main, "21417", ""),
  (125,  45, Euler125.main, "2906969179", ""),
  (126,  18, Euler126.main, "18522", ""),
  (127,  21, Euler127.main, "15377700", ""),
  (128,   9, Euler128.main, "14516824220", ""),
  (129,   0, Euler129.main, "1000023", ""),
  (130,   2, Euler130.main, "149253", ""),
  (131,   0, Euler131.main, "173", ""),
  (132,   1, Euler132.main, "843296", ""),
  (133,   1, Euler133.main, "453647705", ""),
  (134,  13, Euler134.main, "18613426663617118", ""),
  (135,   3, Euler135.main, Euler135.answer, "Number of solutions to x^2-y^2-z^2=n"),
  (136,  26, Euler136.main, "2544559", "Unique solutions to x^2-y^2-z^2=n"),
  (137,   0, Euler137.main, "1120149658760", ""),
  (138,   0, Euler138.main, "1118049290473932", ""),
  (139,   0, Euler139.main, "10057761", ""),
  (140,   0, Euler140.main, "5673835352990", ""),
  (141,  98, Euler141.main, Euler141.answer, "Progressive perfect squares"),
  (142,  59, Euler142.main, "1006193", "Perfect Square Collection"),
  (143,   8, Euler143.main, "25587759", "Torricelli point of a triangle"),
  (144,   0, Euler144.main, "354", ""),
  (145,   0, Euler145.main, "608720", ""),
  (146,  42, Euler146.main, "676333270", "Investigating a Prime Pattern"),
  (147,   0, Euler147.main, "846910284", "Rectangles in cross-hatched grids"),
  (148,   0, Euler148.main, "2129970655314432", "Exploring Pascal's triangle."),
  (149,  73, Euler149.main, Euler149.answer, "Searching for a maximum-sum subsequence"),
  (150, 267, Euler150.main, Euler150.answer, "Minimum-sum sub-triangles"),
  (151,   0, Euler151.main, "0.464398781601087", "Paper sheets of standard sizes"),
  (152,  58, Euler152.main, Euler152.answer, "Writing 1/2 as a sum of inverse squares"),
  (153, 339, Euler153.main, Euler153.answer, "Investigating Gaussian Integers"),
  (154, 804, Euler154.main, "479742450", "Exploring Pascal's pyramid"),
  (155, 145, Euler155.main, Euler155.answer, "Counting Capacitor Circuits"),
  (156,  14, Euler156.main, "21295121502550", "Counting Digits"),
  (157,   0, Euler157.main, "53490", ""),
  (158,   0, Euler158.main, "409511334375", ""),
  (159,  44, Euler159.main, "14489159", "Digital root sums of factorizations."),
  (160,  17, Euler160.main, "16576", "Factorial trailing digits"),
  (161,  10, Euler161.main, "20574308184277971", "Triominoes"),
  (162,   0, Euler162.main, "3D58725572C62302", "Hexadecimal numbers"),
  (163,   0, Euler163.main, "343047", "Cross-hatched triangles"),
  (164,   0, Euler164.main, "378158756814587", ""),
  (165,3096, Euler165.main, Euler165.answer, "Intersections"),
  (166,  18, Euler166.main, "7130034", "Criss Cross"),
  (167,   4, Euler167.main, "3916160068885", "Investigating Ulam sequences"),
  (168,   0, Euler168.main, "59206", "Number Rotations"),
  (169,   0, Euler169.main, "178653872807", ""),
  (170,   3, Euler170.main, "9857164023", ""), -- Pandigital concatenated products
  (171,   2, Euler171.main, "142989277", ""),
  (172,   0, Euler172.main, "227485267000992000", ""),
  (173,   0, Euler173.main, "1572729", ""),
  (174,   1, Euler174.main, "209566", ""),
  (175,   0, Euler175.main, "1,13717420,8", ""),
  (176,   0, Euler176.main, "96818198400000", ""),
  (177,  32, Euler177.main, "129325", ""),
  (178,   0, Euler178.main, "126461847755", ""),
  (179,  43, Euler179.main, "986262", "Consecutive positive divisors"),
  (180,   7, Euler180.main, "285196020571078987", ""),
  (181,  10, Euler181.main, "83735848679360680", "Grouping objects of two different colours"),
  (182,  10, Euler182.main, "399788195976", ""),
  (183,   0, Euler183.main, "48861552", ""),
  (184,   0, Euler184.main, "1725323624056", ""),
  (185, 130, Euler185.main, "4640261571849533", "Number Mind"),
  (186,  30, Euler186.main, "2325629", ""),
  (187,  85, Euler187.main, "17427258", "Semiprimes"),
  (188,   0, Euler188.main, "95962097", ""),
  (189,   7, Euler189.main, "10834893628237824", "Tri-colouring a triangular grid"),
  (190,   0, Euler190.main, "371048281", ""),
  (191,   0, Euler191.main, "1918080160", ""),
  (192,  17, Euler192.main, "57060635927998347", ""),
  (193, 167, Euler193.main, "684465067343069", "Squarefree Numbers"),
  (194,   0, Euler194.main, "61190912", "Coloured Configurations"),
  (195,  41, Euler195.main, "75085391", "Inscribed circles"),
  (196, 620, Euler196.main, Euler196.answer, "Prime triplets"),
  (197,   0, Euler197.main, "1.710637717", ""),
  (198,   4, Euler198.main, "52374425", ""),
  (199,   0, Euler199.main, "0.00396087", "Iterative Circle Packing"),
  (200,  80, Euler200.main, "229161792008", "200th prime-proof sqube"),
  (201,  19, Euler201.main, "115039000", "Subsets with a unique sum"),
  (202,   0, Euler202.main, "1209002624", "Laserbeam"),
  (203,   0, Euler203.main, "34029210557338", "Squarefree Binomial Coefficients"),
  (204,   6, Euler204.main, "2944730", "Generalised Hamming Numbers"),
  (205,   0, Euler205.main, "0.5731441", "Dice Game"),
  (206,   1, Euler206.main, "1389019170", "Concealed Square"),
  (207,   0, Euler207.main, "44043947822", "Integer partition equations"),
  (208,   7, Euler208.main, Euler208.answer, "Robot Walks"),
  (209,   0, Euler209.main, "15964587728784", "Circular Logic"),
  (210, 532, Euler210.main, Euler210.answer, "Obtuse Angled Triangles"),
  (211,  60, Euler211.main, Euler211.answer, "Divisor Square Sum"),
  (212,  25, Euler212.main, "328968937309", "Combined Volume of Cuboids"),
  (213,  28, Euler213.main, Euler213.answer, "Flea Circus"),
  (214,  59, Euler214.main, Euler214.answer, "Totient Chains"),
  (215,   4, Euler215.main, "806844323190414", "Crack-free Walls"),
  (216, 618, Euler216.main, "5437849", "Primality of 2n^2-1"),
  (217,   3, Euler217.main, "6273134", "Balanced Numbers"),
  (218,   0, Euler218.main, "0", "Perfect right-angled triangles"),
  (219,   0, Euler219.main, "64564225042", "Skew-cost coding"),
  (220,   0, Euler220.main, "139776,963904", "Heighway Dragon"),
  (221,  62, Euler221.main, Euler221.answer, "Alexandrian Integers"),
  (222,   0, Euler222.main, "1590933", "Sphere Packing"),
  (223,  63, Euler223.main, Euler223.answer, "Almost right-angled triangles I"),
  (224,   5, Euler224.main, Euler224.answer, "Almost right-angled triangles II"),
  (225,   1, Euler225.main, Euler225.answer, "Tribonacci non-divisors"),
  (226,   0, Euler226.main, "0.11316017", "A Scoop of Blancmange"),
  (227,   1, Euler227.main, Euler227.answer, "The Chase"),
  (228,   0, Euler228.main, "86226", "Minkowski Sums"),
  (229, 113, Euler229.main, Euler229.answer, "Four Representations using Squares")
  ]

total_time :: Int
total_time = sum [ t | (_, t, _, _, _) <- checks ]

main :: IO ()
main = do
  args <- System.getArgs
  let p = parseArgs args
  sequence_ [ check n m s d | (n, l, m, s, d) <- checks, p n l ]
