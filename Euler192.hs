module Euler192 where

{-
Problem 192
03 May 2008

Let x be a real number.

A best approximation to x for the denominator bound d is a rational number
r/s in reduced form, with s d, such that any rational number which is closer
to x than r/s has a denominator larger than d:

|p/q-x| < |r/s-x| ==> q > d

For example, the best approximation to 13 for the denominator bound 20 is
18/5 and the best approximation to 13 for the denominator bound 30 is 101/28.

Find the sum of all denominators of the best approximations to n for the
denominator bound 10^12, where n is not a perfect square and 1 < n <= 100000.
-}

type Z = Integer

-- search based on (semi)convergents of continued fractions

convergents :: [Z] -> [(Z, Z)]
convergents xs = convs (0,1) (1,0) xs
  where
    convs (a,b) (c,d) [] = []
    convs (a,b) (c,d) (x:xs) = (e,f) : convs (c,d) (e,f) xs
      where (e,f) = (a + c*x, b + d*x)

sqrt_cfraction :: (Z, Z) -> [Z]
sqrt_cfraction (r, n) = f 0 1 r
  where
    f p q a = a : f p' q' a'
      where
        p' = q * a - p
        q' = (n - p'^2) `div` q -- why is this always exact?
        a' = (r + p') `div` q'

best_sqrt_approximation :: Z -> (Z, Z) -> (Z, Z)
best_sqrt_approximation dmax (r, n) = convs (0,1) (1,0) xs True
  where
    xs = sqrt_cfraction (r, n)
    -- invariant: t = a%b < c%d
    convs (a,b) (c,d) (x:xs) t
      | f <= dmax = convs (c,d) (e,f) xs $! not t
      | otherwise = best (c, d) (a + c*y, b + d*y) $! not t
      where
        (e,f) = (a + c*x, b + d*x)
        y = (dmax - b) `div` d
    -- invariant: t = a%b < c%d
    best (a,b) (c,d) t
      | 4*b^2*d^2*n < (a*d + b*c)^2 = if t then (a,b) else (c,d)
      | otherwise                   = if t then (c,d) else (a,b)

{-
a/b < sqrt n < c/d

compare (sqrt n - a/b) (c/d - sqrt n)
compare (sqrt n) (a/b + c/d - sqrt n)
compare (2 * sqrt n) (a/b + c/d)
compare (2*b*d * sqrt n) (a*d + b*c)
compare ((2*b*d * sqrt n)^2) ((a*d + b*c)^2)
compare (4*b^2*d^2*n) ((a*d + b*c)^2)
-}

non_squares :: [(Z, Z)]
non_squares = concatMap f [1 ..]
  where f r = [ (r, n) | n <- [r^2 + 1 .. r^2 + 2*r] ]

prob192 :: Z -> Z -> Z
prob192 dmax m =
  sum $
  map (snd . best_sqrt_approximation dmax) $
  takeWhile ((<=m) . snd) non_squares

main :: IO String
main = return $ show $ prob192 (10^12) (10^5)
-- 57060635927998347
