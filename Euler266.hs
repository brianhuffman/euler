module Euler266 where
import Primes
import SquareRoot
import SortedList (unionBy)
import EulerLib (funArray)
import Data.Array

{---------------------------------------------------------------------
Problem 266
Pseudo Square Root

28 November 2009

The divisors of 12 are: 1,2,3,4,6 and 12.  The largest divisor of 12
that does not exceed the square root of 12 is 3.  We shall call the
largest divisor of an integer n that does not exceed the square root
of n the pseudo square root (PSR) of n.  It can be seen that
PSR(3102)=47.

Let p be the product of the primes below 190.
Find PSR(p) mod 10^(16).

---------------------------------------------------------------------}

ps :: [Integer]
ps = takeWhile (<190) primes

n190 :: Integer
n190 = product (takeWhile (<190) primes)

-- brute force solution

psr :: Integer -> Integer
psr n = last (takeWhile (<=r) ds)
  where
    ds = list_divisors n
    r = square_root n

divisors :: [Integer] -> [Integer]
divisors ps = foldr f [1] ps
  where
    n = product ps
    r = square_root n
    f p ds = unionBy (flip compare) ds ds'
      where ds' = dropWhile (>r) (map (*p) ds)

{---------------------------------------------------------------------

let r = floor (sqrt n), so n = r^2 + s, with 0 <= s < 2r+1
n = (r-x)*(r+x+e)
n = (r-x)*(r+x) + (r-x)*e
n = r^2 - x^2 - ex + re
s = -x^2 - ex + re
0 = -x^2 - ex + (re-s)

e +/- sqrt [e^2 + 4(re-s)]
-------------------------- = x
         -2

x = (sqrt (e^2 + 4(re-s)) - e) / 2

We will search for a suitable e, starting from 1.

---------------------------------------------------------------------}

find_e n es = filter ok es
  where
    (r, s) = square_root_aux n
    ok e = is_square (e^2 + 4*(r*e-s))

-- has a space leak!
psr' :: Integer -> Integer
psr' n = e `seq` (r-x)
  where
    (r, s) = square_root_aux n
    ok e = is_square (e^2 + 4*(r*e-s))
    e = head (filter ok [1..])
    x = (square_root (e^2 + 4*(r*e-s)) - e) `div` 2

{---------------------------------------------------------------------

There are 42 primes below 190.

So N has 2^42 distinct factors!

Split into two parts, each with 21 factors.
List all divisors of each, one list ascending, one descending.
Traverse lists, looking for products near sqrt(n).

---------------------------------------------------------------------}

unsplice :: [a] -> ([a], [a])
unsplice [] = ([], [])
unsplice (x : xs) = (x : zs, ys)
  where (ys, zs) = unsplice xs

psr2 :: Integer -> Integer
psr2 n = go 1 ds1 ds2
  where
    r = square_root n
    (ps1, ps2) = unsplice (prime_factorization n)
    a = product [ p^e | (p,e) <- ps1 ]
    b = product [ p^e | (p,e) <- ps2 ]
    ds1 = list_divisors_of_pf ps1
    ds2 = [ b `div` d | d <- list_divisors_of_pf ps2 ]
    go m xs@(x : xs') ys@(y : ys')
      | z > r = go m xs ys'
      | z > m = go z xs' ys
      | otherwise = go m xs' ys
      where z = x*y
    go m _ _ = m

main :: IO String
main = return $ show $ psr2 n190 `mod` (10^16)

answer :: String
answer = "1096883702440585"

-- 2323218950659046189161096883702440585
