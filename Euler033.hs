module Euler033 where
import Data.Ratio

------------------------------------------------------------------------------
-- 33. Discover all the fractions with an unorthodox cancelling method.

cancel_fractions =
  [ (p, q) |
    a <- [1..9],
    b <- [1..9],
    a /= b,
    c <- [1..9],
    let p = 10*a + b,
    let q = 10*b + c,
    p*c == a*q ]
-- 16/64, 19/95, 26/65, 49/98

main :: IO String
main =
  return $ show $ denominator $
  product [ p % q | (p,q) <- cancel_fractions ]
-- 100

