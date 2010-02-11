module Euler271 where
import Primes (chinese)

{---------------------------------------------------------------------
Problem 271
Modular Cubes, part 1

02 January 2010

For a positive number n, define S(n) as the sum of the integers x, for
which 1<x<n and x^(3)≡1 mod n.

When n=91, there are 8 possible values for x, namely : 9, 16, 22, 29,
53, 74, 79, 81.  Thus, S(91)=9+16+22+29+53+74+79+81=363.

Find S(13082761331670030).

---------------------------------------------------------------------}

-- cubert
rs :: Integer -> [Integer]
rs n = [ x | x <- [1 .. n-1], (x^3) `mod` n == 1 ]

s :: Integer -> Integer
s n = sum [ x | x <- [2 .. n-1], (x^3) `mod` n == 1 ]

ps :: [Integer]
ps = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43]

rs_prod [] = [(0, 1)]
rs_prod (p : ps) =
  [ chinese (r,p) (a,n) |
    r <- rs p,
    (a,n) <- rs_prod ps
  ]

s_prod ps = sum (map fst (rs_prod ps)) - 1

main :: IO String
main = return $ show $ s_prod ps

answer :: String
answer = "4617456485273129588"


{---------------------------------------------------------------------

rs(p) = [1] for p <- [2,3,5,11,17,23,29,41]

rs(7) = [1,2,4]
rs(13) = [1,3,9]
rs(19) = [1,7,11]
rs(31) = [1,5,25]
rs(37) = [1,10,26]
rs(43) = [1,6,36]

13082761331670030: 2 3 5 7 11 13 17 19 23 29 31 37 41 43

---------------------------------------------------------------------}

