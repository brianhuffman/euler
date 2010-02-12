module Euler272 where
import Primes

{---------------------------------------------------------------------
Problem 272
Modular Cubes, part 2

02 January 2010

For a positive number n, define C(n) as the number of the integers x,
for which 1<x<n and x^(3)≡1 mod n.

When n=91, there are 8 possible values for x, namely : 9, 16, 22, 29,
53, 74, 79, 81.  Thus, C(91)=8.

Find the sum of the positive numbers n≤10^(11) for which C(n)=242.

---------------------------------------------------------------------}

rs :: Integer -> [Integer]
rs n = [ x | x <- [1 .. n-1], (x^3) `mod` n == 1 ]


{---------------------------------------------------------------------

A prime (or prime power) may have either 1 or 3 cube roots of 1.

If C(n)=242, n must have at least 5 separate prime powers in its
factorization, each of which having three cube roots of 1.

For primes p == 1 (mod 6), p^k has three cube roots of 1.
For primes p == 5 (mod 6), 1 is the only cube root of 1 (mod p^k).

Special case: There are three cube roots of 1 (mod 3^k), for k>1.

rs(p) = [1] for p <- [2,3,5,11,17,23,29,41,47,53,59,71,83,89,...

rs(p) = [1,x,y] for p <- [7,13,19,31,37,43,61,67,73,79,97,...

The smallest possible number would be:

7*13*19*31*37 = 1,983,163

---------------------------------------------------------------------}

type N = Integer

products :: Int -> N -> [N]
products k nmax = go k nmax primes
  where
    go k nmax ps
      | k < 0 = []
      | product (take (max 1 k) ps) > nmax = if k == 0 then [1] else []
    go k nmax (p : ps) =
      go k nmax ps ++
      [ pe * x |
        let pemax = nmax `div` product (take (k-1) ps),
        pe <- takeWhile (<= pemax) (iterate (*p) p),
        let k' = case p `mod` 6 of
                   1 -> k-1
                   5 -> k
                   3 -> if pe>3 then k-1 else k
                   _ -> k,
        x <- go k' (nmax `div` pe) ps
      ]

-- sum_products k nmax = sum (products k nmax)
sum_products :: Int -> N -> N
sum_products k nmax = go k nmax primes
  where
    go k nmax ps
      | k < 0 = 0
      | product (take (max 1 k) ps) > nmax = if k == 0 then 1 else 0
    go k nmax (p : ps) =
      go k nmax ps + sum
      [ pe * go k' (nmax `div` pe) ps |
        let pemax = nmax `div` product (take (k-1) ps),
        pe <- takeWhile (<= pemax) (iterate (*p) p),
        let k' = case p `mod` 6 of
                   1 -> k-1
                   5 -> k
                   3 -> if pe>3 then k-1 else k
                   _ -> k
      ]

main :: IO String
main = return $ show $ sum_products 5 (10^11)

answer :: String
answer = "8495585919506151122"
