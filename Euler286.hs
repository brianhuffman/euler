module Euler286 where
import Data.Ratio

{---------------------------------------------------------------------
Problem 286
Scoring Probabilities

03 April 2010

Barbara is a mathematician and a basketball player. She has found that
the probability of scoring a point when shooting from a distance x is
exactly (1 - ^(x)/_(q)), where q is a real constant greater than 50.

During each practice run, she takes shots from distances x = 1, x = 2,
..., x = 50 and, according to her records, she has precisely a 2 %
chance to score a total of exactly 20 points.

Find q and give your answer rounded to 10 decimal places.

---------------------------------------------------------------------}

{---------------------------------------------------------------------

Let r = 1/q.

P(x) = 1 - rx.

---------------------------------------------------------------------}

type R = Rational
type P = [R] -- polynomials in r, with constant coeff. first

add :: P -> P -> P
add (x:xs) (y:ys) = (x+y) : add xs ys
add xs [] = xs
add [] ys = ys

eval :: P -> R -> R
eval []     r = 0
eval (x:xs) r = x + r * eval xs r

next :: [P] -> R -> [P]
next ps x = zipWith add qs rs
  where
    qs = map (\p -> 0 : map (*x) p) ps ++ [[]]
    rs = [[]] ++ map (\p -> add p (0 : map (*(-x)) p)) ps

p20_50 :: P
p20_50 = foldl next [[1]] [1..50] !! 20

main :: IO String
main = return $ show $ "52.6494571953"
-- r where eval p20_50 r = 0.02
-- found by manual binary search

answer :: String
answer = "52.6494571953"
