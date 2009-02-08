module Euler159 where
import Primes
import Data.Array

{-
Problem 159
Digital root sums of factorisations.

30 June 2007

A composite number can be factored many different ways. For instance,
not including multiplication by one, 24 can be factored in 7 distinct
ways:

24 = 2x2x2x3
24 = 2x3x4
24 = 2x2x6
24 = 4x6
24 = 3x8
24 = 2x12
24 = 24

Recall that the digital root of a number, in base 10, is found by
adding together the digits of that number, and repeating that process
until a number is arrived at that is less than 10. Thus the digital
root of 467 is 8.

We shall call a Digital Root Sum (DRS) the sum of the digital roots of
the individual factors of our number.

The chart below demonstrates all of the DRS values for 24.

Factorisation	Digital Root Sum
2x2x2x3         9
2x3x4           9
2x2x6           10
4x6             10
3x8             11
2x12            5
24              6

The maximum Digital Root Sum of 24 is 11.  The function mdrs(n) gives
the maximum Digital Root Sum of n. So mdrs(24)=11.  Find SUM mdrs(n)
for 1 < n < 1,000,000.

-}

{-
Analysis:

Note that DRS(n) is determined by n mod 9.

multiplication table mod 9
  | 1 2 3 4 5 6 7 8 9
--+---------------------
1 | 1 2 3 4 5 6 7 8 9
2 | 2 4 6 8 1 3 5 7 9
3 | 3 6 9 3 6 9 3 6 9
4 | 4 8 3 7 2 6 1 5 9
5 | 5 1 6 2 7 3 8 4 9
6 | 6 3 9 6 3 9 6 3 9
7 | 7 5 3 1 8 6 4 2 9
8 | 8 7 6 5 4 3 2 1 9
9 | 9 9 9 9 9 9 9 9 9

favorable to replace:
(2,3) -> 6  (+1)
(2,4) -> 8  (+2)
(3,3) -> 9  (+3)
(2,2,2) -> 8  (+2)

equal to replace:
(2,2) -> 4  (+0)
(3,6) -> 9  (+0)
-}

-- digital root
droot :: Int -> Int
droot x = let r = x `mod` 9 in if r == 0 then 9 else r

pf_array :: Int -> Array Int [Int]
pf_array m = a
  where
    a = listArray (1, m) (map f [1 .. m])
    f 1 = []
    f n = droot p : a!q
      where
        p = least_prime_divisor n
        q = n `div` p

-- maximum digital root sum
-- (argument given as list of digital roots of prime factors)
mdrs_pf :: [Int] -> Int
mdrs_pf ds = sum ds + bonus
  where
    arr = accumArray (+) 0 (1,9) [ (d, 1) | d <- ds ]
    n2 = arr!2
    n3 = arr!3
    n4 = arr!4
    (b1,n3') = divMod n3 2        -- (3,3) -> 9  (bonus +3)
    b2 = min n2 n4                -- (2,4) -> 8  (bonus +2)
    (b3,n2') = divMod (n2 - b2) 3 -- (2,2,2) -> 8  (bonus +2)
    b4 = min n2' n3'              -- (2,3) -> 6  (bonus +1)
    bonus = 3*b1 + 2*b2 + 2*b3 + b4

-- maximum digital root sum
mdrs :: Int -> Int
mdrs n = mdrs_pf
  [ d | (p,e) <- prime_factorization n, d <- replicate e (droot p) ]

prob159 :: Int -> Int
prob159 n = sum $ map mdrs_pf $ elems $ pf_array (n-1)

main :: IO String
main = return $ show $ prob159 (10^6)

answer :: String
answer = "14489159"
