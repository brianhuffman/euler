{-# LANGUAGE FlexibleContexts #-}

module Euler251 where
import Primes
import Permutation
import EulerLib
import SquareRoot (square_root)

import Data.Array.IArray
import Data.Array (Array)
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed (UArray)


{-
Problem 251
Cardano Triplets

20 June 2009

A triplet of positive integers (a,b,c) is called a Cardano Triplet if
it satisfies the condition:

cuberoot(a + b * sqrt(c)) + cuberoot(a - b * sqrt(c)) = 1

For example, (2,1,5) is a Cardano Triplet.

There exist 149 Cardano Triplets for which a+b+c ≤ 1000.

Find how many Cardano Triplets exist such that a+b+c ≤ 100,000,000.

-}

{-

cuberoot(a + b * sqrt(c)) + cuberoot(a - b * sqrt(c)) = 1

let x = cuberoot(a + b * sqrt(c))
let y = cuberoot(a - b * sqrt(c))
let X = x^3
let Y = y^3

(x + y) = 1
(x + y)^3 = 1
X + 3xxy + 3xyy + Y = 1
X + 3xy(x + y) + Y = 1
X + 3xy + Y = 1
3xy = 1 - (X + Y)
(3xy)^3 = (1 - (X + Y))^3
27XY = 1 - 3(X+Y) + 3(X+Y)^2 - (X+Y)^3

X = a + b sqrt c
Y = a - b sqrt c
X+Y = 2a
XY = (a + b sqrt c) (a - b sqrt c) = a^2 - b^2 c

27(a^2 - b^2 c) = 1 - 6a + 12a^2 - 8a^3
27 a^2 - 27 b^2 c = 1 - 6a + 12a^2 - 8a^3
8a^3 + 27 a^2 - 27 b^2 c = 1 - 6a + 12a^2
8a^3 + 15 a^2 - 27 b^2 c = 1 - 6a
8a^3 + 15 a^2 + 6a - 27 b^2 c = 1
8a^3 + 15 a^2 + 6a - 1 - 27 b^2 c = 0
8a^3 + 15 a^2 + 6a - 1 = 27 b^2 c

It must be that a == 2 (mod 3).

8a^3 + 15 a^2 + 6a - 1 = 27 b^2 c

let a = 3n+2.

8a^3 + 15 a^2 + 6a - 1 =
8(3n+2)^3 + 15(3n+2)^2 + 6(3n+2) - 1
8(27n^3 + 54n^2 + 36n + 8) + 15(9n^2 + 12n + 4) + 6(3n + 2) - 1
216n^3 + 567n^2 + 486n + 135
27(8n^3 + 21n^2 + 18n + 5)

8n^3 + 21n^2 + 18n + 5 = b^2 c
where a = 3n+2

(n + 1)^2 (8n + 5) = b^2 c
where a = 3n+2

(3n+2, n+1, 8n+5) is a Cardano triplet.

General solution:
(a,b,c) = (3n+2, (p/q)(n+1), (q^2/p^2)(8n+5))

8n+5 is never a multiple of 4.
3^2 | 8n+5 <--> n == 5 (mod 3^2)
5^2 | 8n+5 <--> n == 15 (mod 5^2)
7^2 | 8n+5 <--> n == 30 (mod 7^2)
9^2 | 8n+5 <--> n == 50 (mod 9^2)
11^2 | 8n+5 <--> n == 75 (mod 11^2)
...
(2k+1)^2 | 8n+5 <--> n == 5*k(k+1)/2 (mod (2k+1)^2)

8n+5 is never itself a square.

-}

type Z = Int

{-
square_factor :: Z -> Z
square_factor n =
  product [ (p ^ (e `div` 2)) | (p, e) <- prime_factorization n ]
-}

mult_pf pf1 [] = pf1
mult_pf [] pf2 = pf2
mult_pf pf1@((p,i):pf1') pf2@((q,j):pf2') =
  case compare p q of
    LT -> (p,i) : mult_pf pf1' pf2
    GT -> (q,j) : mult_pf pf1 pf2'
    EQ -> (p,i+j) : mult_pf pf1' pf2'

-- b^2 * c = (n+1)^2 * (8*n + 5)
-- b + c > 2.5 n

triplets :: Z -> [(Z, Z, Z)]
triplets abcmax =
  [ (a, b, c) |
    n <- [0 .. nmax],
    let a = 3*n + 2,
    -- b^2 * c = (n+1)^2 * (8*n + 5)
--    let pf1 = prime_factorization (n + 1),
--    let pf2 = [ (p, e `div` 2) | (p, e) <- prime_factorization (8*n + 5) ],
    let pf1 = pf (n + 1),
    let pf2 = [ (p, e `div` 2) | (p, e) <- pf (8*n + 5) ],
{-
    let pf3 = mult_pf pf1 pf2,
    b <- list_divisors_of_pf' pf3,
    let c = b2c `div` (b^2),
-}
    let qs = list_divisors_of_pf' pf1,
    let ps = list_divisors_of_pf' pf2,
    q <- qs,
    q < square_root abcmax,
    p <- ps,
    gcd p q == 1,
    (n+1) `div` q <= abcmax `div` p,
    let b = ((n+1) `div` q) * p,
    (8*n+5) `div` p^2 <= abcmax `div` q^2,
    let c = ((8*n+5) `div` p^2) * q^2,
    a + b + c <= abcmax
  ]
  where
    nmax = (2 * abcmax) `div` 11
    d = odd_factor_array ((8*nmax + 5)`div`2)
    pf 1 = []
    pf n = (p, e) : pf r
      where
        p = if even n then 2 else d!(n`div`2)
        (r, e) = divN n p -- n = r * p^e
    divN :: Int -> Int -> (Int, Int)
    divN x p
      | r == 0 = let (z, n) = divN q p in (z, n+1)
      | otherwise = (x, 0)
      where (q, r) = divMod x p

list_divisors_of_pf' :: [(Int, Int)] -> [Int]
list_divisors_of_pf' [] = [1]
list_divisors_of_pf' ((p,e):pes) =
  [ p^n * d | n <- [0 .. e],  d <- list_divisors_of_pf pes ]

prob251 nmax = length (triplets nmax)
-- prob251 (10^3) = 149
-- prob251 (10^4) = 1632
-- prob251 (10^5) = 16916
-- prob251 (10^6) = 171128
-- prob251 (10^7) = 1719479
-- prob251 (10^8) = 17222999


is_cardano (a,b,c) = lhs == rhs
  where
    lhs = 27 * (a^2 - b^2 * c)
    rhs = (1 - 2*a)^3

main :: IO String
main = return $ show $ prob251 (10^8)

answer :: String
answer = "17222999"


-------------------------------------

factor_array :: Int -> UArray Int Int
factor_array m =
  runSTUArray (do
    a <- newArray_ (1, m)
    -- a[n] <- n
    mapM_ (\n -> writeArray a n n) [1 .. m]
    -- a[n] <- a prime factor of n
    mapM_ (check a) (takeWhile (\n -> n*n <= m) [2 ..])
    return a
  )
  where
    check a n = do
      d <- readArray a n
      if d /= n then return () else do
      mapM_ (\k -> writeArray a k n) [n^2, n^2+n .. m]


-- odd_factor_array m ! n = factor_array m ! (2*n+1)
odd_factor_array :: Int -> UArray Int Int
odd_factor_array m =
  runSTUArray (do
    a <- newArray (1, m) 0
    mapM_ (\n -> writeArray a n (2*n+1)) [1 .. m]
    mapM_ (check a) (takeWhile (\n -> 2*n*(n+1) <= m) [1 ..])
    return a
  )
  where
    check a n = do
      let k = 2*n+1
      let clear i = writeArray a i k
      p <- readArray a n
      if p == k then mapM_ clear [2*n*(n+1), 2*n*(n+1)+k .. m]
           else return ()
