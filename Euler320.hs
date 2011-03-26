module Euler320 where
import Primes

{---------------------------------------------------------------------

Problem 320
Factorials divisible by a huge integer
15 January 2011

Let N(i) be the smallest integer n such that n! is divisible by
(i!)^1234567890

Let S(u)=∑N(i) for 10 ≤ i ≤ u.

S(1000)=614538266565663.

Find S(1 000 000) mod 10^18.

---------------------------------------------------------------------}

-- Function exponent_in_factorial from 154, 160, 231, 288:
exponent_in_factorial :: Integer -> Integer -> Integer
exponent_in_factorial p n =
  if n == 0 then 0 else
  let m = n `div` p in m + exponent_in_factorial p m

{---------------------------------------------------------------------

N(1) = 1
N(2) = LEAST n. 2^1234567890 | n!

exponent_in_factorial 2 1234567903 = 1234567888
exponent_in_factorial 2 1234567904 = 1234567893

N(2) = 1234567904

N(3) = LEAST n. 6^1234567890 | n!

exponent_in_factorial 3 2469135797 = 1234567889
exponent_in_factorial 3 2469135798 = 1234567893

N(3) = 2469135798

----------------------------------------------------------------------
Theorem: exponent_in_factorial p n ≤ n/(p-1)

By induction:
Case (n = 0): exponent_in_factorial p 0 = 0 ≤ n/(p-1)
Case (n = m*p + r, where 0 ≤ r < p):
Assume exponent_in_factorial p m ≤ m/(p-1)
exponent_in_factorial p n
  = m + exponent_in_factorial p m
  = m + exponent_in_factorial p m
  ≤ m + m/(p-1)
  = m*p/(p-1)
  ≤ (m*p + r)/(p-1)
  = n/(p-1)

----------------------------------------------------------------------

We need a way of calculating the inverse of exponent_in_factorial.

Given m, what is the least n such that m ≤ exponent_in_factorial p n?

m ≤ exponent_in_factorial p n ≤ n/(p-1)
m ≤ n/(p-1)
m⋅(p-1) ≤ n

---------------------------------------------------------------------}

big :: Integer
big = 1234567890

least_factorial_slow p m = search_from (m*(p-1))
  where
    search_from n
      | m <= exponent_in_factorial p n = n
      | otherwise = search_from (n+1)

{---------------------------------------------------------------------
exponent_in_factorial p (a*p^k)
  = a * (1 + p + p^2 + ... p^(k-1))
  = a * repunit p k
  = a * ((p^k)-1)/(p-1)

exponent_in_factorial p (a0 + a1*p + a2*p^2 + .. ak*p^k)
  = SUM i=0..k, ai * repunit p i

---------------------------------------------------------------------}

repunit b n = f 0 n
  where
    f x 0 = x
    f x n = f (x*b+1) (n-1)

least_factorial p m0 = f u0 v0 m0
  where
    -- largest (repunit p k, p^k) below m
    find_repunit u v
      | m0 < u' = (u, v)
      | otherwise = find_repunit u' (p*v)
      where u' = u*p+1
    (u0, v0) = find_repunit 0 1
    f 0 v m = 0
    f u v m = q*v + f (div u p) (div v p) m'
      where (q, m') = divMod m u

type PF = [(Int, Int)]

mult_pf :: PF -> PF -> PF
mult_pf pf1 [] = pf1
mult_pf [] pf2 = pf2
mult_pf pf1@((p,i):pf1') pf2@((q,j):pf2') =
  case compare p q of
    LT -> (p,i) : mult_pf pf1' pf2
    GT -> (q,j) : mult_pf pf1 pf2'
    EQ -> (p,i+j) : mult_pf pf1' pf2'

prime_factorization_factorial :: Int -> PF
prime_factorization_factorial 0 = []
prime_factorization_factorial n =
  mult_pf (prime_factorization n) (prime_factorization_factorial (n-1))

-- least n such that (i!)^b | n!
prob320n :: Integer -> Int -> Integer
prob320n b i = maximum (map f (prime_factorization_factorial i))
  where f (p, e) = least_factorial (toInteger p) (b * toInteger e)

prob320n_pf :: Integer -> PF -> Integer
prob320n_pf b pf = maximum (map f pf)
  where f (p, e) = least_factorial (toInteger p) (b * toInteger e)

lookups :: PF -> [Int] -> PF
lookups pf@((p,e):pf') qs@(q:qs') =
  case compare p q of
    EQ -> (p,e) : lookups pf' qs'
    LT -> lookups pf' qs
    GT -> lookups pf qs'
lookups _ _ = []

prob320s :: Integer -> Int -> Integer
prob320s b u = total pf9 9 0 0
  where
    pf9 = prime_factorization (product [1..9])
    total pf i n s
      | i' < u = total pf' i' n' (s + n')
      | otherwise = s + n'
      where
        i' = i+1
        pfi = prime_factorization (i+1)
        pf' = mult_pf pf pfi
        -- only look up those primes whose exponents have changed
        n' = max n (prob320n_pf b (lookups pf' (map fst pfi)))

main :: IO String
main = return $ show $ prob320s big (10^6) `mod` (10^18)

answer :: String
answer = "278157919195482643"
