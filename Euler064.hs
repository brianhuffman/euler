module Euler064 where
import ContinuedFraction

------------------------------------------------------------------------------
-- 64. How many continued fractions for N ≤ 10000 have an odd period?
{-
sqrt x
n + (sqrt x - n)
n + 1 / (1 / (sqrt x - n))

z / (sqrt x - y)
z (sqrt x + y) / (sqrt x - y)(sqrt x + y)
z (sqrt x + y) / (x - y^2)

z / (sqrt x - y) = z (sqrt x + y) / (x - y^2)

(sqrt n + x) / y  -  t
(sqrt n - (t*y - x)) / y

y / (sqrt n - (t*y - x))
y (sqrt n + (t*y - x)) / (sqrt n - (t*y - x)) (sqrt n + (t*y - x))
y (sqrt n + (t*y - x)) / (n - (t*y - x)^2)
-}
{-
prob64a n = (t0, f t0 (n - t0^2))
  where
    root = sqrt (fromIntegral n)
    t0 = floor root
    -- (root + x) / y = t + (root - x') / y
    -- y / (root - x') = y (root + x') / (n - x'^2)
    f x 0 = []
    f x y = t : if y == 1 then [] else f x' y'
      where
        t = floor ((root + fromIntegral x) / fromIntegral y)
        x' = t*y - x
        y' = (n - x'^2) `div` y

prob64b m = [ n | n <- [1 .. m], odd (length (snd (prob64a n))) ]
-}

prob64 m =
  [ n | n <- [1 .. m], odd (length (snd (sqrt_cfraction_cycle n))) ]

main :: IO String
main = return $ show $ length $ prob64 10000
-- 1322

{-
Odd-length cycles:

length 1:
  sqrt_cfrac_cycle (n^2 + 1) = (n, [2n])

length 3:
  sqrt_cfrac_cycle (25n^2 + 14n + 2) = (5n+1, [2,2,10n+2])
  sqrt_cfrac_cycle (...) = (17n+2, [4,4,34n+4])

(370,(19,[4,4,38]))
(1313,(36,[4,4,72]))
(2834,(53,[4,4,106]))
(4933,(70,[4,4,140]))
(7610,(87,[4,4,174]))
(10865,(104,[4,4,208]))

(1613,(40,[6,6,80]))
(5954,(77,[6,6,154]))

(4778,(69,[8,8,138]))

(11257,(106,[10,10,212]))

length 5:
  sqrt_cfrac_cycle (...) = (5n+3, [1,1,1,1,10n+6])

  sqrt_cfrac_cycle (...) = (2k+1, [k,1,1,k,4k+2])

  sqrt_cfrac_cycle (...) = (5n+3, [1,1,1,1,10n+6])
  sqrt_cfrac_cycle (...) = (13n+5, [2,1,1,2,26n+10])
  sqrt_cfrac_cycle (...) = (25n+7, [3,1,1,3,50n+14])
  sqrt_cfrac_cycle (...) = (41n+9, [4,1,1,4,82n+18])
  sqrt_cfrac_cycle (...) = (61n+11, [5,1,1,5,122n+22])

  sqrt_cfrac_cycle (...) =
    ((2k^2+2k+1)n+2k+1, [k,1,1,k,(4k^2+4k+2)n+4k+2])
-}
