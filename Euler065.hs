module Euler065 where
import ContinuedFraction
import Data.Char

------------------------------------------------------------------------------
-- 65. Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e.
{-
continued_fraction [] = 0
continued_fraction [n] = n%1
continued_fraction (n:ns) = n%1 + recip (continued_fraction ns)
-}

e_cfrac = 2 : concatMap (\n -> [1, 2*n, 1]) [1 ..]

prob65 n = cfraction (take n e_cfrac)

main :: IO String
main = return $ show $ sum $ map digitToInt $ show $ fst (prob65 100)
-- 272
