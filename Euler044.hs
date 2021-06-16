module Euler044 where
import EulerLib
import qualified SortedList as S
import Data.List

------------------------------------------------------------------------------
-- 44. Find the smallest pair of pentagonal numbers whose sum and difference is pentagonal.

{-
P(a+b) = P(a) + P(b) + 3ab

P(n) = n(3n - 1)/2 = (3n^2 - n)/2
P(a+b) = (3(a+b)^2 - (a+b)) / 2
P(a+b) = (3(a^2 + b^2 + 2ab) - (a+b)) / 2
P(a+b) = (3a^2 + 3b^2 + 6ab - a - b) / 2
P(a+b) = (3a^2 - a)/2 + (3b^2 - b)/2 + 6ab/2
P(a+b) = P(a) + P(b) + 3ab
-}

{-
P(a) == a  (mod 3)
-}

{-
pentagonal triples (a,b,c) such that P(a) + P(b) = P(c)
let k = c-b
(a,b,b+k) such that P(a) + P(b) = P(b+k)
(a,b,b+k) such that P(a) + P(b) = P(b) + P(k) + 3bk
(a,b,b+k) such that P(a) = P(k) + 3bk
(a,b,b+k) such that P(a) - P(k) = 3bk
-}

pentagon :: Int -> Int
pentagon n = n*(3*n-1) `div` 2

-- (a,b,c) such that pentagon a + pentagon b = pentagon c
pentagonal_triples :: [(Int, Int, Int)]
pentagonal_triples =
  [ (a, b, b+k) |
    a <- [1 ..],
    k <- [a-3, a-6 .. 1],
    let (b, r) = (pentagon a - pentagon k) `divMod` (3 * k),
    r == 0 ]

prob44a =
  map (map (\(a,b,c) -> (pentagon a, pentagon b, pentagon c))) $
  filter (not . null . tail) $
  groupBy (\(a,b,c) (d,e,f) -> a == d && c == e) $
  pentagonal_triples

-- x = P(1020), y-x = P(1912), y = P(2167), x+y = P(2395)
-- 1560090, 5482660, 7042750, 8602840

main :: IO String
main = return $ show $ snd3 $ head $ head prob44a

{-
pentagonal numbers have the form n(3n-1)/2
y-x, x, y, x+y all pentagonal
w, x, w+x, w+2x all pentagonal, where w = y-x

let P(a) = w = y-x
    P(b) = x
    P(c) = y = x+w
    P(d) = z = x+2w

P(a) + P(b) = P(c)
P(b) + P(c) = P(d)

a == P(a)  (mod 3)
P(a+b) = P(a) + P(b) + 3ab

-}
