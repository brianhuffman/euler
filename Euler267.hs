module Euler267 where
import EulerLib
import Data.Ratio

{---------------------------------------------------------------------
Problem 267
Billionaire

04 December 2009

You are given a unique investment opportunity.

Starting with £1 of capital, you can choose a fixed proportion, f, of
your capital to bet on a fair coin toss repeatedly for 1000 tosses.

Your return is double your bet for heads and you lose your bet for
tails.

For example, if f = 1/4, for the first toss you bet £0.25, and if
heads comes up you win £0.5 and so then have £1.5. You then bet £0.375
and if the second toss is tails, you have £1.125.

Choosing f to maximize your chances of having at least £1,000,000,000
after 1,000 flips, what is the chance that you become a billionaire?

All computations are assumed to be exact (no rounding), but give your
answer rounded to 12 digits behind the decimal point in the form
0.abcdefghijkl.

---------------------------------------------------------------------}


{---------------------------------------------------------------------

On a win, your capital is multiplied by (1+f).
On a loss, your capital is multiplied by (1-f).

After W wins and L losses, the final capital is:

C = (1+2f)^W * (1-f)^L

log(C) = log( (1+2f)^W * (1-f)^L )
       = W log(1+2f) + L log(1-f)

1e9 = (1+2f)^W * (1-f)^L

On a win, log(capital) changes by log(1+2f) (which is positive).
On a loss, log(capital) changes by log(1-f) (which is negative).

---------------------------------------------------------------------}

log1e9 :: Double
log1e9 = log 1e9

log_final :: Double -> Int -> Double
log_final f w = fromIntegral w * log(1+2*f) + fromIntegral l * log(1-f)
  where l = 1000 - w

final :: Double -> Int -> Double
final f w = (1+2*f)^w * (1-f)^l
  where l = 1000 - w

percentage_needed :: Double -> Double
percentage_needed f = (target - log_l) / (log_w - log_l)
  where
    log_w = log (1+2*f)
    log_l = log (1-f)
    target = log 1e9 / 1000
    -- p * log_w + (1-p) * log_l = target
    -- p * log_w + log_l - p * log_l = target
    -- p * (log_w - log_l) + log_l = target
    -- p * (log_w - log_l) = target - log_l
    -- p = (target - log_l) / (log_w - log_l)

-- Seems to reach a minimum at about f = 0.147
-- At this value of f, we need at least 432 wins to be a billionaire.

choose_at_least :: Int -> Int -> Integer
choose_at_least n k = sum (drop k (pascal_triangle !! n))

prob267 :: Rational
prob267 = choose_at_least 1000 432 % (2^1000)

main :: IO String
main = return $ showFloat 12 prob267

answer :: String
answer = "0.999992836187"
-- 0.abcdefghijkl
-- 0.000000000059 WRONG!
-- 0.000000000090 WRONG!
-- 0.000000000030 WRONG!

