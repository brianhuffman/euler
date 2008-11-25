module Euler025 where
import EulerLib

------------------------------------------------------------------------------
-- 25. What is the first term in the Fibonacci sequence to contain 1000 digits?

-- F(n) = phi^n - (1-phi)^n / sqrt 5
-- F(n) ~~ phi^n / sqrt 5
-- ln(F(n) ~~ n (ln phi) - (ln 5) / 2
-- ln(10^999) = 999 ln 10 = 2300.2825

-- 10^999 <= F(n)
-- ln(10^999) <= ln(F(n))
-- 999 ln 10 <= n (ln phi) - ln 5 / 2
-- (999 ln 10 + ln 5 / 2) / ln phi <= n

golden_ratio = (1 + sqrt 5) / 2
prob25a n = ((n - 1) * log 10 + log 5 / 2) / log golden_ratio

num_fibs_below :: Integer -> Int
num_fibs_below m = f 0 0 1
  where
    f n a b = if m <= a then n else ((f $! (n+1)) $! b) $! (a+b)

prob25 :: Int -> Int
prob25 n = num_fibs_below (10^(n-1))
-- prob25 1000 = 4782

main :: IO String
main = return $ show $ prob25 1000
