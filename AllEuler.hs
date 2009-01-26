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

check :: Int -> IO String -> String -> IO ()
check n action answer =
  do putStrLn (show n ++ ": ")
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
Skip:
*154: Exploring Pascal's pyramid
*170: Largest pandigital formed by concatenating products

Slow:
150: (214 s) Sub-triangle having minimum sum
155: (145 s) Counting capacitor circuits
193: (89 s) Squarefree numbers
165: (67 s) Intersections
60: (43 s) Concatenating pairs of primes
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

checks :: [(Int, Int, IO String, String)]
checks = [
  (  1,   0, Euler001.main, "233168"),
  (  2,   0, Euler002.main, "4613732"),
  (  3,   0, Euler003.main, "6857"),
  (  4,   0, Euler004.main, "906609"),
  (  5,   0, Euler005.main, "232792560"),
  (  6,   0, Euler006.main, "25164150"),
  (  7,   1, Euler007.main, "104743"),
  (  8,   0, Euler008.main, "40824"),
  (  9,   0, Euler009.main, "31875000"),
  ( 10,   1, Euler010.main, "142913828922"),
  ( 11,   0, Euler011.main, "70600674"),
  ( 12,   2, Euler012.main, "76576500"),
  ( 13,   0, Euler013.main, "5537376230"),
  ( 14,  12, Euler014.main, "837799"),
  ( 15,   0, Euler015.main, "137846528820"),
  ( 16,   0, Euler016.main, "1366"),
  ( 17,   0, Euler017.main, "21124"),
  ( 18,   0, Euler018.main, "1074"),
  ( 19,   0, Euler019.main, "171"),
  ( 20,   0, Euler020.main, "648"),
  ( 21,   2, Euler021.main, "31626"),
  ( 22,   2, Euler022.main, "871198282"),
  ( 23,   8, Euler023.main, "4179871"),
  ( 24,   0, Euler024.main, "2783915460"),
  ( 25,   0, Euler025.main, "4782"),
  ( 26,   0, Euler026.main, "983"),
  ( 27,   4, Euler027.main, "-59231"),
  ( 28,   0, Euler028.main, "669171001"),
  ( 29,   1, Euler029.main, "9183"),
  ( 30,   1, Euler030.main, "443839"),
  ( 31,   0, Euler031.main, "73682"),
  ( 32,   1, Euler032.main, "45228"),
  ( 33,   0, Euler033.main, "100"),
  ( 34,   1, Euler034.main, "40730"),
  ( 35,   2, Euler035.main, "55"),
  ( 36,   0, Euler036.main, "872187"),
  ( 37,   5, Euler037.main, "748317"),
  ( 38,   0, Euler038.main, "932718654"),
  ( 39,   0, Euler039.main, "840"),
  ( 40,   2, Euler040.main, "210"),
  ( 41,   0, Euler041.main, "7652413"),
  ( 42,   1, Euler042.main, "162"),
  ( 43,   0, Euler043.main, "16695334890"),
  ( 44,   1, Euler044.main, "5482660"),
  ( 45,   1, Euler045.main, "1533776805"),
  ( 46,   0, Euler046.main, "5777"),
  ( 47,   3, Euler047.main, "134043"),
  ( 48,   0, Euler048.main, "9110846700"),
  ( 49,   3, Euler049.main, "296962999629"),
  ( 50,   0, Euler050.main, "997651"),
  ( 51,   4, Euler051.main, "121313"),
  ( 52,   0, Euler052.main, "142857"),
  ( 53,   0, Euler053.main, "4075"),
  ( 54,   1, Euler054.main, "376"),
  ( 55,   4, Euler055.main, "249"),
  ( 56,   2, Euler056.main, "972"),
  ( 57,   1, Euler057.main, "153"),
  ( 58,   9, Euler058.main, "26241"),
  ( 59,   0, Euler059.main, "107359"),
  ( 60, 102, Euler060.main, "26033"),
  ( 61,   0, Euler061.main, "28684"),
  ( 62,   3, Euler062.main, "127035954683"),
  ( 63,   0, Euler063.main, "49"),
  ( 64,   3, Euler064.main, "1322"),
  ( 65,   0, Euler065.main, "272"),
  ( 66,   1, Euler066.main, "661"),
  ( 67,   1, Euler067.main, "7273"),
  ( 68,   0, Euler068.main, "6531031914842725"),
  ( 69,   0, Euler069.main, "510510"),
  ( 70,   2, Euler070.main, "8319823"),
  ( 71,   0, Euler071.main, "428570"),
  ( 72,   5, Euler072.main, "303963552391"),
  ( 73,   2, Euler073.main, "5066251"),
  ( 74,   9, Euler074.main, "402"),
  ( 75,   9, Euler075.main, "214954"),
  ( 76,   0, Euler076.main, "190569291"),
  ( 77,   0, Euler077.main, "71"),
  ( 78,   9, Euler078.main, "55374"),
  ( 79,   0, Euler079.main, "73162890"),
  ( 80,   1, Euler080.main, "40886"),
  ( 81,   1, Euler081.main, "427337"),
  ( 82,   1, Euler082.main, "260324"),
  ( 83,   8, Euler083.main, "425185"),
  ( 84,   0, Euler084.main, "101524"),
  ( 85,   0, Euler085.main, "2772"),
  ( 86,   0, Euler086.main, "1818"),
  ( 87,   9, Euler087.main, "1097343"),
  ( 88,   9, Euler088.main, "7587457"),
  ( 89,   0, Euler089.main, "743"),
  ( 90,   0, Euler090.main, "1217"),
  ( 91,   3, Euler091.main, "14234"),
  ( 92,   0, Euler092.main, "8581146"),
  ( 93,   8, Euler093.main, "1258"),
  ( 94,   0, Euler094.main, "518408346"),
  ( 95,  14, Euler095.main, "14316"),
  ( 96,  15, Euler096.main, "24702"),
  ( 97,   0, Euler097.main, "8739992577"),
  ( 98,   1, Euler098.main, "18769"),
  ( 99,   1, Euler099.main, "709"),
  (100,   0, Euler100.main, "756872327473"),
  (101,   0, Euler101.main, "37076114526"),
  (102,   1, Euler102.main, "228"),
  (103,   6, Euler103.main, "20313839404245"),
  (104,   7, Euler104.main, "329468"),
  (105,   2, Euler105.main, "73702"),
  (106,   0, Euler106.main, "21384"),
  (107,   0, Euler107.main, "259679"),
  (108,   0, Euler108.main, "180180"),
  (109,   0, Euler109.main, "38182"),
  (110,   4, Euler110.main, "9350130049860600"),
  (111,   6, Euler111.main, "612407567715"),
  (112,   7, Euler112.main, "1587000"),
  (113,   0, Euler113.main, "51161058134250"),
  (114,   0, Euler114.main, "16475640049"),
  (115,   0, Euler115.main, "168"),
  (116,   0, Euler116.main, "20492570929"),
  (117,   0, Euler117.main, "100808458960497"),
  (118,  43, Euler118.main, "44680"),
  (119,   0, Euler119.main, "248155780267521"),
  (120,   0, Euler120.main, "333082500"),
  (121,   0, Euler121.main, "2269"),
  (122,  17, Euler122.main, "1582"),
  (123,   2, Euler123.main, "21035"),
  (124,  15, Euler124.main, "21417"),
  (125,  45, Euler125.main, "2906969179"),
  (126,  18, Euler126.main, "18522"),
  (127,  21, Euler127.main, "15377700"),
  (128,   9, Euler128.main, "14516824220"),
  (129,   0, Euler129.main, "1000023"),
  (130,   2, Euler130.main, "149253"),
  (131,   0, Euler131.main, "173"),
  (132,   1, Euler132.main, "843296"),
  (133,   1, Euler133.main, "453647705"),
  (134,  13, Euler134.main, "18613426663617118"),
  (135, 101, Euler135.main, "4989"),
  (136,  26, Euler136.main, "2544559"), -- Unique solutions of x^2-y^2-z^2=n
  (137,   0, Euler137.main, "1120149658760"),
  (138,   0, Euler138.main, "1118049290473932"),
  (139,   0, Euler139.main, "10057761"),
  (140,   0, Euler140.main, "5673835352990"),
  (141, 235, Euler141.main, "878454337159"),
  (142,  59, Euler142.main, "1006193"),
  (143,   8, Euler143.main, "25587759"),
  (144,   0, Euler144.main, "354"),
  (145,   0, Euler145.main, "608720"),
  (146,  42, Euler146.main, "676333270"),
  (147,   0, Euler147.main, "846910284"),
  (148,   0, Euler148.main, "2129970655314432"),
  (149,  83, Euler149.main, "52852124"),
  (150, 267, Euler150.main, Euler150.answer), -- Sub-triangle sums
  (151,   0, Euler151.main, "0.464398781601087"),
  (152, 143, Euler152.main, "301"),
  (153, 339, Euler153.main, Euler153.answer), -- Factors of Gaussian integers
  (154, 804, Euler154.main, "479742450"), -- (80 s) Pascal's pyramid
  (155,1110, Euler155.main, "3857447"), --(160 s) Capacitor circuits
  (156,  14, Euler156.main, "21295121502550"),
  (157,   0, Euler157.main, "53490"),
  (158,   0, Euler158.main, "409511334375"),
  (159,  44, Euler159.main, "14489159"),
  (160,  17, Euler160.main, "16576"),
  (161,  10, Euler161.main, "20574308184277971"),
  (162,   0, Euler162.main, "3D58725572C62302"),
  (163,   0, Euler163.main, "343047"),
  (164,   0, Euler164.main, "378158756814587"),
  (165,3096, Euler165.main, Euler165.answer), -- Intersections
  (166,  18, Euler166.main, "7130034"),
  (167,   4, Euler167.main, "3916160068885"),
  (168,   0, Euler168.main, "59206"),
  (169,   0, Euler169.main, "178653872807"),
  (170,   3, Euler170.main, "9857164023"), -- Pandigital concatenated products
  (171,   2, Euler171.main, "142989277"),
  (172,   0, Euler172.main, "227485267000992000"),
  (173,   0, Euler173.main, "1572729"),
  (174,   1, Euler174.main, "209566"),
  (175,   0, Euler175.main, "1,13717420,8"),
  (176,   0, Euler176.main, "96818198400000"),
  (177,  32, Euler177.main, "129325"),
  (178,   0, Euler178.main, "126461847755"),
  (179,  43, Euler179.main, "986262"),
  (180,   7, Euler180.main, "285196020571078987"),
  (181,  10, Euler181.main, "83735848679360680"),
  (182,  10, Euler182.main, "399788195976"),
  (183,   0, Euler183.main, "48861552"),
  (184,   0, Euler184.main, "1725323624056"),
  (185, 130, Euler185.main, "4640261571849533"),
  (186,  30, Euler186.main, "2325629"),
  (187,  85, Euler187.main, "17427258"),
  (188,   0, Euler188.main, "95962097"),
  (189,   7, Euler189.main, "10834893628237824"),
  (190,   0, Euler190.main, "371048281"),
  (191,   0, Euler191.main, "1918080160"),
  (192,  17, Euler192.main, "57060635927998347"),
  (193, 167, Euler193.main, "684465067343069"), -- Squarefree numbers
  (194,   0, Euler194.main, "61190912"),
  (195,  41, Euler195.main, "75085391"), -- Inscribed circles
  (196,1196, Euler196.main, "322303240771079935"), -- Prime triplets
  (197,   0, Euler197.main, "1.710637717"),
  (198,   4, Euler198.main, "52374425"),
  (199,   0, Euler199.main, "0.00396087"),
  (200,  80, Euler200.main, "229161792008"),
  (201,  19, Euler201.main, "115039000"),
  (202,   0, Euler202.main, "1209002624"),
  (203,   0, Euler203.main, "34029210557338"),
  (204,   6, Euler204.main, "2944730"),
  (205,   0, Euler205.main, "0.5731441"),
  (206,   1, Euler206.main, "1389019170"),
  (207,   0, Euler207.main, "44043947822"),
  (208,  66, Euler208.main, "331951449665644800"), -- Robot walks
  (209,   0, Euler209.main, "15964587728784"),
  (210, 532, Euler210.main, Euler210.answer), -- Obtuse Angled Triangles
  (211,  60, Euler211.main, Euler211.answer), -- Divisor Square Sum
  (212,  25, Euler212.main, "328968937309"),
  (213,  28, Euler213.main, Euler213.answer), -- Flea Circus
  (214, 209, Euler214.main, "1677366278943"), -- Totient chains
  (215,   4, Euler215.main, "806844323190414"),
  (216, 923, Euler216.main, "5437849"),
  (217,   3, Euler217.main, "6273134"),
  (218,   0, Euler218.main, "0"),
  (219,   0, Euler219.main, "64564225042"),
  (220,   0, Euler220.main, "139776,963904"),
  -- missing: 221
  (222,   0, Euler222.main, "1590933"),
  -- missing: 223
  (224, 999, Euler224.main, "4137330"),
  -- missing: 225
  (226,   0, Euler226.main, "0.11316017"),
  -- missing: 227
  (228,   0, Euler228.main, "86226"),
  (229,9999, Euler229.main, "11325263")
  ]

total_time :: Int
total_time = sum [ t | (_, t, _, _) <- checks ]

main :: IO ()
main = do
  args <- System.getArgs
  let p = parseArgs args
  sequence_ [ check n m s | (n, l, m, s) <- checks, p n l ]
