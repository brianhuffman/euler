module Euler259 where
import Data.Array
import EulerLib (funArray)
import qualified SortedList as S
import Data.List (insert)
import Data.Ratio (numerator, denominator)

{-
Problem 259
Reachable Numbers

10 October 2009

A positive integer will be called reachable if it can result from an
arithmetic expression obeying the following rules:

    * Uses the digits 1 through 9, in that order and exactly once each.

    * Any successive digits can be concatenated (for example, using
      the digits 2, 3 and 4 we obtain the number 234).

    * Only the four usual binary arithmetic operations (addition,
      subtraction, multiplication and division) are allowed.

    * Each operation can be used any number of times, or not at all.

    * Unary minus is not allowed.

    * Any number of (possibly nested) parentheses may be used to
      define the order of operations.

For example, 42 is reachable, since (1/23) * ((4*5)-6) * (78-9) = 42.

What is the sum of all positive reachable integers?

-}

type Q = Rational

concat_digits :: Int -> Int -> Q
concat_digits i j
  | i > j = 0
  | otherwise = 10 * concat_digits i (j-1) + fromIntegral j

reachables :: Array (Int, Int) [Q]
reachables = a
  where
    a = funArray ((1,1),(9,9)) f
    f (i, j) =
      insert (concat_digits i j) $
      foldl S.union [] $
      [ op xs ys |
        j' <- [i .. j - 1],
        let i' = j' + 1,
        let xs = a!(i, j'),
        let ys = a!(i', j),
        op <- [adds, subs, muls, divs]
      ]

-- e = a | m
reachable_e :: Array (Int, Int) [Q]
reachable_e = a
  where
    a = funArray ((1,1),(9,9)) f
    f (i, j) =
      S.union
        (reachable_a ! (i, j))
        (reachable_m ! (i, j))

-- a = e + m | e - m | n
reachable_a :: Array (Int, Int) [Q]
reachable_a = funArray ((1,1),(9,9)) f
  where
    f (i, j) =
      insert (concat_digits i j) $
      foldl S.union [] $
      [ op xs ys |
        j' <- [i .. j - 1],
        let i' = j' + 1,
        let xs = reachable_e!(i, j'),
        let ys = reachable_m!(i', j),
        op <- [adds, subs]
      ]

-- m = e * a | e / a | n
reachable_m :: Array (Int, Int) [Q]
reachable_m = funArray ((1,1),(9,9)) f
  where
    f (i, j) =
      insert (concat_digits i j) $
      foldl S.union [] $
      [ op xs ys |
        j' <- [i .. j - 1],
        let i' = j' + 1,
        let xs = reachable_e!(i, j'),
        let ys = reachable_a!(i', j),
        op <- [muls, divs]
      ]

-- precondition: sorted input
-- postcondition: sorted output

adds' :: [Q] -> [Q] -> [Q]
adds' xs ys = foldl S.union []
  [ [ x + y | x <- xs ] | y <- ys ]

adds :: [Q] -> [Q] -> [Q]
adds xs ys
  | length xs < length ys = adds' ys xs
  | otherwise = adds' xs ys

subs :: [Q] -> [Q] -> [Q]
subs xs ys = adds xs (reverse (map negate ys))

muls' :: [Q] -> [Q] -> [Q]
muls' xs ys = foldl S.union []
  [ [ x * y | x <- case compare y 0 of
                     GT -> xs
                     EQ -> [0]
                     LT -> reverse xs
    ]
    | y <- ys ]

muls :: [Q] -> [Q] -> [Q]
muls xs ys
  | length xs < length ys =  muls' ys xs
  | otherwise = muls' xs ys

invs :: [Q] -> [Q]
invs xs = map recip (reverse ys ++ reverse (dropWhile (== 0) zs))
  where (ys, zs) = span (< 0) xs

divs :: [Q] -> [Q] -> [Q]
divs xs ys = muls xs (invs ys)

whole_numbers :: [Q] -> [Integer]
whole_numbers xs =
  [ numerator x | x <- xs, denominator x == 1 ]

main :: IO String
main =
  return $ show $ sum $
  filter (>0) $ whole_numbers $ reachable_e ! (1,9)

answer :: String
answer = "20101196798"

{-

(1,3) 28  (1,4) 163  (1,5) 1060  (1,6)  6620  (1,7) 52950  (1,8) 393390
(2,4) 30  (2,5) 205  (2,6) 1357  (2,7) 10398  (2,8) 75246  (2,9) 605600
(3,5) 32  (3,6) 217  (3,7) 1649  (3,8) 12070  (3,9) 94735
(4,6) 32  (4,7) 233  (4,8) 1751  (4,9) 13722
(5,7) 32  (5,8) 236  (5,9) 1851
(6,8) 32  (6,9) 243
(7,9) 32

We can reduce computation by adding some more distinctions to the grammar.

Old grammar:

e = e + e | e - e | e * e | e / e | n

New grammar:

Addition/subtraction must associate to the left.  I.e. the right
argument to (+) or (-) must not be another (+) or (-).  Similarly for
multiplication/division.

x + (y + z) --> (x + y) + z
x - (y + z) --> (x - y) - z
x + (y - z) --> (x + y) - z
x - (y - z) --> (x - y) + z

e = a | m
a = e + m | e - m | n
m = e * a | e / a | n

-}
