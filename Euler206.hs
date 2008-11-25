module Euler206 where

{-
Find the unique positive integer whose square has the form
1_2_3_4_5_6_7_8_9_0, where each “_” is a single digit.
-}

{-
1010101011 <= n <= 1389026623

Obviously, the last digit of n must be zero.
Thus, n is a multiple of 10.
Thus, n^2 is a multiple of 100.

Reduces to the problem:
Find the unique m whose square has the form 1_2_3_4_5_6_7_8_9,
where each “_” is a single digit.

m <= 138902662

The last digit of m must be 3 or 7.

[ (n, n2) |
  d
  n <- [1..999],
  let n2 = n^2 `mod` 1000,
  n2 `mod` 10 == 9,
  n2 `div` 100 == 8 ]

[(43,849),(53,809),(83,889),(167,889),(197,809),(207,849),(293,849),(303,809),(333,889),(417,889),(447,809),(457,849),(543,849),(553,809),(583,889),(667,889),(697,809),(707,849),(793,849),(803,809),(833,889),(917,889),(947,809),(957,849)]


 [(n, n2) | n <- [3,13..999], let n2 = n^2 `mod` 1000, n2 `div` 100 == 8]

[(43,849),(53,809),(83,889),(293,849),(303,809),(333,889),(543,849),(553,809),(583,889),(793,849),(803,809),(833,889)]

(1_2_3_4_5_6_7_8_9_0 = ..........^2)
(last digit must be 0)
(1_2_3_4_5_6_7_8_9_0 = .........0^2)
(1_2_3_4_5_6_7_8_900 = .........0^2)
(last digit must be 3 or 7)
(1_2_3_4_5_6_7_8_900 = ........30^2)
-}

mmax = 138902662

prob206a =
  [ n |
    q <- [0 .. 100],
    r <- [3, 7],
    let n = q*10 + r,
    let n2 = n^2 `mod` 1000,
    n2 `div` 100 == 8 ]

prob206b =
  [ n |
    q <- [0 .. 100],
    r <- prob206a,
    let n = q*(10^3) + r,
    let n2 = n^2 `mod` (10^5),
    n2 `div` (10^4) == 7 ]

prob206c =
  [ n |
    q <- [0 .. 100],
    r <- prob206b,
    let n = q*(10^5) + r,
    let n2 = n^2 `mod` (10^7),
    n2 `div` (10^6) == 6 ]

prob206d = takeWhile (<= mmax)
  [ n |
    q <- [0 .. 100],
    r <- prob206c,
    let n = q*(10^7) + r,
    let n2 = n^2 `mod` (10^9),
    n2 `div` (10^8) == 5 ]

prob206e = takeWhile (<= mmax)
  [ n |
    q <- [0 .. 100],
    r <- prob206d,
    let n = q*(10^9) + r,
    let n2 = n^2 `mod` (10^11),
    n2 `div` (10^10) == 4 ]

prob206f = takeWhile (<= mmax)
  [ n |
    q <- [0 .. 100],
    r <- prob206e,
    let n = q*(10^11) + r,
    let n2 = n^2 `mod` (10^13),
    n2 `div` (10^12) == 3 ]

prob206g = takeWhile (<= mmax)
  [ n |
    q <- [0 .. 100],
    r <- prob206f,
    let n = q*(10^13) + r,
    let n2 = n^2 `mod` (10^15),
    n2 `div` (10^14) == 2 ]

prob206h = takeWhile (<= mmax)
  [ n |
    q <- [0 .. 100],
    r <- prob206g,
    let n = q*(10^15) + r,
    let n2 = n^2 `mod` (10^17),
    n2 `div` (10^16) == 1 ]

prob206 = head prob206h * 10

main :: IO String
main = return $ show $ prob206
