module ContinuedFraction where

{-
continued_fraction [] = (1,0)
continued_fraction (x:xs) =
  let (a,b) = continued_fraction xs in (a*x + b, a)
-}

-- evaluate finite continued fraction to (numerator, denominator)
cfraction :: (Num a) => [a] -> (a, a)
cfraction xs = cfrac 0 1 1 0 xs
  where
    cfrac a b c d [] = (c,d)
    cfrac a b c d (x:xs) = cfrac c d (a + c*x) (b+d*x) xs

-- equivalent formulation of cfraction
cfraction' [] = (1, 0)
cfraction' (n:ns) =
  let (x, y) = cfraction' ns in (n*x + y, x)

-- give continued fraction representation of rational
ratio_cfraction :: (Integral a) => (a, a) -> [a]
ratio_cfraction (_,0) = []
ratio_cfraction (a,b) = q : ratio_cfraction (b,r)
  where (q,r) = a `divMod` b

-- convergents xs = map cfraction (tail (inits xs))
convergents :: (Num a) => [a] -> [(a, a)]
convergents xs = convs (0,1) (1,0) xs
  where
    convs (a,b) (c,d) [] = []
    convs (a,b) (c,d) (x:xs) = (e,f) : convs (c,d) (e,f) xs
      where (e,f) = (a + c*x, b + d*x)

convergent_numerators :: (Num a) => [a] -> [a]
convergent_numerators = convergent_seq 0 1

convergent_denominators :: (Num a) => [a] -> [a]
convergent_denominators = convergent_seq 1 0

convergent_seq :: (Num a) => a -> a -> [a] -> [a]
convergent_seq a b [] = []
convergent_seq a b (x:xs) = c : convergent_seq b c xs
  where c = a + b*x

{-
continued fraction of x =
  a; continued fraction of (1 / (x - a))
  where a = floor x

1 / (x - a), where x = (sqrt n + p)/q
1 / ((sqrt n + p)/q - a)
1 / ((sqrt n + p)/q - q*a/q)
1 / ((sqrt n - (q*a-p))/q)
q / (sqrt n - (q*a-p))
q*(sqrt n + (q*a-p)) / (sqrt n - (q*a-p))(sqrt n + (q*a-p))
q*(sqrt n + (q*a-p)) / (n - (q*a-p)^2)
(sqrt n + (q*a-p)) / ((n - (q*a-p)^2)/q)

floor ((sqrt n + p) / q)
= floor (sqrt n + p) `div` q
= (floor (sqrt n) + p) `div` q

p' = q*a - p
q' = (n - p'^2) / q
a' = (floor (sqrt n) + p') `div` q'
-}

sqrt_cfraction :: (Integral a) => a -> [a]
sqrt_cfraction n = f 0 1 r
  where
    r = isqrt n
    f p q a = a : f p' q' a'
      where
        p' = q * a - p
        q' = (n - p'^2) `div` q -- why is this always exact?
        a' = (r + p') `div` q'

sqrt_cfraction_cycle :: (Integral a) => a -> (a, [a])
sqrt_cfraction_cycle n = (r, f r r (n - r^2))
  where
    r = isqrt n
    f a p 0 = []
    f a p 1 = [r + p]
    f a p q = a' : f a' p' q'
      where
        a' = (r + p) `div` q
        p' = q * a' - p
        q' = (n - p'^2) `div` q

isqrt :: (Integral a) => a -> a
isqrt x = f x
  where
    f r = let r' = ((r + x `div` r) `div` 2)
          in if r <= r' then r else f r'
