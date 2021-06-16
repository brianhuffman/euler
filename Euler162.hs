module Euler162 where
import Data.Array

{-
Problem 162

05 October 2007

In the hexadecimal number system numbers are represented using 16 different
digits:

0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F

The hexadecimal number AF when written in the decimal number system equals
10 * 16 + 15 = 175.

In the 3-digit hexadecimal numbers 10A, 1A0, A10, and A01 the digits 0,1 and
A are all present.

Like numbers written in base ten we write hexadecimal numbers without leading
zeroes.

How many hexadecimal numbers containing at most sixteen hexadecimal digits
exist with all of the digits 0,1, and A present at least once?

Give your answer as a hexadecimal number.

(A,B,C,D,E and F in upper case, without any leading or trailing code that marks
the number as hexadecimal and without leading zeroes , e.g. 1A3F and not: 1a3f
and not 0x1a3f and not $1A3F and not #1A3F and not 0000001A3F)
-}

show_hex = reverse . f
  where
    a = listArray (0,15) "0123456789ABCDEF"
    f 0 = ""
    f n = let (q,r) = divMod n 16 in a!r : f q

{-
P(a|b) = P(a) + P(b) - P(a&b)
P(a|b|c) = P(a) + P(b) + P(c) - P(b&c) - P(a&c) - P(a&b) + P(a&b&c)

exactly n digits: 15 * 16^(n-1)
exactly n digits, with no 0: 15 * 15^(n-1)
exactly n digits, with no 1: 14 * 15^(n-1)
exactly n digits, with no A: 14 * 15^(n-1)
exactly n digits, with no 1,A: 13 * 14^(n-1)
exactly n digits, with no 0,A: 14 * 14^(n-1)
exactly n digits, with no 0,1: 14 * 14^(n-1)
exactly n digits, with no 0,1,A: 13 * 13^(n-1)

exactly n digits, with ~0 or ~1 or ~A:
(15+14+14)*15^(n-1) - (13+14+14)*14^(n-1) + 13*13^(n-1)
-}

prob162 m = sum $ map g [3 .. m]
  where
    f n = 43*15^(n-1) - 41*14^(n-1) + 13*13^(n-1)
    g n = 15*16^(n-1) - f n

main :: IO String
main = return $ show_hex $ prob162 16
-- 3D58725572C62302
