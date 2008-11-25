module Euler100 where

------------------------------------------------------------------------------
-- 100. Finding the number of blue discs for which there is 50% chance of taking two blue.
{-
15 blue, 21 total: P(BB) = (15/21) * (14/20) = 1/2
85 blue, 120 total: P(BB) = (85/120) * (84/119) = 1/2
b/t * (b-1)/(t-1) = 1/2
2*b*(b-1) = t*(t-1)
2b^2 - t^2 - 2b + t = 0

find solution for least t > 10^12
b > 707106781186.5475

(b,t)
-----
(1,1)
(3,4)
(15,21)
(85,120)
(493,697)
(2871,4060)
(16731,23661)
(97513,137904)

b' = 3b + 2t - 2
t' = 4b + 3t - 3
-}

continued_fraction_expansion :: Double -> [Integer]
continued_fraction_expansion = f
  where f x = let (n, x') = properFraction x in n : f (recip x')

prob100a = f 1 1
  where f b t = (b,t) : f (3*b + 2*t - 2) (4*b + 3*t - 3)

prob100 n = fst $ head $ dropWhile ((<= n) . snd) $ prob100a

main :: IO String
main = return $ show $ prob100 (10^12)
-- 756872327473

