module Euler063 where

------------------------------------------------------------------------------
-- 63. How many n-digit positive integers exist which are also an nth power?

{-
10^(n-1) <= a^n < 10^n
obviously, a < 10.
log (10^(n-1)) <= log(a^n)
(n-1) log (10) <= n log(a)
(n-1)/n <= log(a)/log(10)
1 - 1/n <= log(a)/log(10)
1 - log(a)/log(10) <= 1/n
n <= 1 / (1 - log(a)/log(10))
-}

-- n such that a^n has n digits
prob63a a = takeWhile (\n -> 10^(n-1) <= a^n) [1 ..]

main :: IO String
main = return $ show $ sum $ map (length . prob63a) [1 .. 9]
-- 49
