module Euler198 where
import Ratio
import Word
import Int

-- 198. Ambiguous Numbers
{-
A best approximation to a real number x for the denominator bound d is
a rational number r/s (in reduced form) with s ≤ d, so that any rational
number p/q which is closer to x than r/s has q > d.

Usually the best approximation to a real number is uniquely determined
for all denominator bounds. However, there are some exceptions, e.g. 9/40
has the two best approximations 1/4 and 1/5 for the denominator bound 6.
We shall call a real number x ambiguous, if there is at least one
denominator bound for which x possesses two best approximations. Clearly,
an ambiguous number is necessarily rational.

How many ambiguous numbers x = p/q, 0 < x < 1/100, are there whose
denominator q does not exceed 10^8?
-}

{-
(a/b + c/d) / 2
= (ad/bd + bc/bd) / 2
= (ad+bc)/2bd
-}

-- ambiguous_numbers :: Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
ambiguous_numbers m (a, b) (c, d)
  | y > m = []
  | a == 0 = ambiguous_numbers m (a, b) (p, q)
                ++ ambiguous_numbers m (p, q) (c, d)
  | otherwise = ((a,b),(x,y),(c,d)) :
                ambiguous_numbers m (a, b) (p, q) ++
                ambiguous_numbers m (p, q) (c, d)
  where
    (p, q) = (a + c, b + d)
    (x, y) = (a*d + b*c, 2*b*d)

ambiguous_ratios m (a, b) (c, d)
  | b+d > m = []
  | a == 0 || denominator x > m =
              ambiguous_ratios m (a, b) (p, q)
                ++ ambiguous_ratios m (p, q) (c, d)
  | otherwise = x :
                ambiguous_ratios m (a, b) (p, q) ++
                ambiguous_ratios m (p, q) (c, d)
  where
    (p, q) = (a + c, b + d)
    x = (a*d + b*c)%(2*b*d)

type T = Int64
-- count_ambiguous :: T -> (T, T) -> (T, T) -> Int -> Int
{-
count_ambiguous :: T -> (T, T) -> (T, T) -> Int -> Int
count_ambiguous m (a, b) (c, d) k
  | y > m = k
  | otherwise = count_ambiguous m (p, q) (c, d) $!
                count_ambiguous m (a, b) (p, q) $! k'
  where
    (p, q) = (a + c, b + d)
    (x, y) = (a*d + b*c, 2*b*d)
    k' = if (100*x < y) then k+1 else k
-}
-- count_ambiguous m a b = how many ambiguous numbers p/q
-- with 1/a < p/q < 1/b, and q <= m
count_ambiguous m a b x
  | 2*a*b > m = x
  | otherwise = count_ambiguous m (a+b) b $!
                count_ambiguous m a (a+b) $! (x+1)

{-
c = 2*a*b

(a,b,c) -> (a+b, b, c + 2bb)
(a,b,c) -> (a, a+b, c + 2aa)

-}


-- prob198 d b = how many ambiguous numbers p/q
-- with 0 < p/q < 1/b, and q <= d
prob198 :: Int64 -> Int64 -> Int64
prob198 d b =
  sum (takeWhile (>0) [count_ambiguous d (n+1) n 0 | n <- [b ..] ])
  + d `div` 2 - b `div` 2

main :: IO String
main = return $ show $ prob198 (10^8) 100

{-
count_ambiguous (10^5) (0,1) (1,100) 0 = 321
count_ambiguous (10^6) (0,1) (1,100) 0 = 9813
count_ambiguous (10^7) (0,1) (1,100) 0 = 167512
count_ambiguous (2*10^7) (0,1) (1,100) 0 = 377103
count_ambiguous (3*10^7) (0,1) (1,100) 0 = 602581
count_ambiguous (4*10^7) (0,1) (1,100) 0 = 838422
count_ambiguous (5*10^7) (0,1) (1,100) 0 = 1081899
count_ambiguous (6*10^7) (0,1) (1,100) 0 = 1331528
count_ambiguous (7*10^7) (0,1) (1,100) 0 = 1586291
count_ambiguous (8*10^7) (0,1) (1,100) 0 = 
count_ambiguous (9*10^7) (0,1) (1,100) 0 = 
count_ambiguous (10^8) (0,1) (1,100) 0 =

let xs = [count_ambiguous (10^8) (1,n+1) (1,n) 0 | n <- [100..7071] ]

2374475 -- WRONG!

 2374475 +
49999901 =
52374376 -- WRONG!
(also need to count 1/102, 1/104 .. 1/198)

2374475 + 49999950 = 52374425 -- RIGHT!
-}