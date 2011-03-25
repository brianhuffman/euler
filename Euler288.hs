module Euler288 where

{-

Problem 288
An enormous factorial
17 April 2010

For any prime p the number N(p,q) is defined by N(p,q) = ∑n=0 to q
T[n]*p^n with T[n] generated by the following random number generator:

S[0] = 290797
S[n+1] = S[n]^2 mod 50515093
T[n] = S[n] mod p

Let Nfac(p,q) be the factorial of N(p,q).
Let NF(p,q) be the number of factors p in Nfac(p,q).

You are given that NF(3,10000) mod 3^20=624955285.

Find NF(61,10^7) mod 61^10

-}

ts :: Int -> [Int]
ts p = [ fromIntegral s `mod` p | s <- iterate next s0 ]
  where
    s0 = (290797 :: Integer)
    next s = (s^2) `mod` 50515093

-- Function exponent_in_factorial from 154, 160, 231:
exponent_in_factorial :: Integer -> Integer -> Integer
exponent_in_factorial p n =
  if n == 0 then 0 else
  let m = n `div` p in m + exponent_in_factorial p m

from_base :: Int -> [Int] -> Integer
from_base b = foldr f 0
  where
    b' = toInteger b
    f d x = x * b' + toInteger d

nn :: Int -> Int -> Integer
nn p q = from_base p (take (q+1) (ts p))

nf :: Int -> Int -> Integer
nf p q = exponent_in_factorial (toInteger p) (nn p q)

nf_mod :: Int -> Int -> Int -> Integer
nf_mod p q k = (nf p q) `mod` (toInteger p ^ k)

-- nf_mod 3 10000 20 = 624955285

{-

However, we will just use the base-p representation in a list, instead
of using actual integers.

-}

-- exponent_in_factorial p (from_base p ds) `mod` (p^k)
exponent_in_factorial_digits :: Int -> Int -> [Int] -> Integer
exponent_in_factorial_digits p k ds = (f 0 ds) `mod` (toInteger p^k)
  where
    f total [] = total
    f total (d:ds) = f (total + from_base p (take k ds)) ds

nf_mod' :: Int -> Int -> Int -> Integer
nf_mod' p q k = exponent_in_factorial_digits p k ds
  where ds = take (q+1) (ts p)

main :: IO String
main = return $ show $ nf_mod' 61 (10^7) 10

answer :: String
answer = "605857431263981935"