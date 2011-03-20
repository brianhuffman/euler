module Euler284 where

{-

Problem 284
Steady Squares
27 March 2010

The 3-digit number 376 in the decimal numbering system is an example
of numbers with the special property that its square ends with the
same digits: 3762 = 141376. Let's call a number with this property a
steady square.

Steady squares can also be observed in other numbering systems. In the
base 14 numbering system, the 3-digit number c37 is also a steady
square: c372 = aa0c37, and the sum of its digits is c+3+7=18 in the
same numbering system. The letters a, b, c and d are used for the 10,
11, 12 and 13 digits respectively, in a manner similar to the
hexadecimal numbering system.

For 1 ≤ n ≤ 9, the sum of the digits of all the n-digit steady squares
in the base 14 numbering system is 2d8 (582 decimal). Steady squares
with leading 0's are not allowed.

Find the sum of the digits of all the n-digit steady squares in the
base 14 numbering system for 1 ≤ n ≤ 10000 (decimal) and give your
answer in the base 14 system using lower case letters where necessary.

-}

{-

base 10:
376^2 = 141376
9376^2 = (9*1000 + 376)^2 = (9^2 * 1000^2 + 2*9*1000*376 + 376^2)

(9,376)^2 = (9*1000 + 376)^2 = (9^2 * 1000^2 + 2*9*1000*376 + 376^2)

376^2 = (141,376)
(9,376)^2 = (9,000 + 376)^2 = (9,000,000 + (2*9*376),000 + 376^2)
(9,376)^2 = (9,000 + 376)^2 = (9,000,000 + (2*9*376),000 + 141,376)
-- we already know the last 3 digits will match!
(9,376)^2 / 1,000 = 9,000 + (2*9*376) + 141

(9376^2 - 376^2) = (9376 + 376) * (9376 - 376)

376^2 - 376 = 141000

(9376^2 - 9376) = 

-}


base14 :: Integer -> String
base14 = f []
  where
   f ds 0 = ds
   f ds n = f (("0123456789abcd" !! fromIntegral r) : ds) q
     where (q, r) = n `divMod` 14

steady_squares b nmax = f nmax
  where
    f 1 = [ x | x <- [1..b-1], (x^2-x) `mod` b == 0 ]
    f n = ys ++ zs
      where
        ys = f (n-1)
        zs = [ z | x <- [1..b-1], y <- ys,
               let z = b^(n-1)*x+y, (z^2-z)`mod`(b^n) == 0 ]

-- paired with digit sums
steady_squares_slow b nmax = f nmax
  where
    f 1 = [ (x, x) | x <- [1..b-1], (x^2-x) `mod` b == 0 ]
    f n = ys ++ zs
      where
        ys = f (n-1)
        zs = [ (z, x+y') | x <- [1..b-1], (y,y') <- ys,
               let z = b^(n-1)*x+y, (z^2-z)`mod`(b^n) == 0 ]

-- paired with digit sums
steady_squares' b nmax =
  [ (x, xsum) |
    (bn, xs) <- take nmax (iterate next (b, xs1)),
    let bn' = bn `div` b,
    (x, xsum) <- xs,
    x >= bn' ] -- filter out leading zeroes
  where
    xs1 = [ (x, x) | x <- [0..b-1], (x^2-x) `mod` b == 0 ]
    next (bn, xs) = (bn', xs')
      where
        bn' = b * bn
        xs' = [ (y, xsum + d) |
                d <- [0..b-1],
                (x, xsum) <- xs,
                let y = bn*d+x,
                (y^2-y) `mod` bn' == 0 ]

prob284 :: Integer -> Int -> Integer
prob284 b nmax = sum (map snd (steady_squares' b nmax))

main :: IO String
main = return $ base14 $ prob284 14 10000

answer :: String
answer = "5a411d7b"
