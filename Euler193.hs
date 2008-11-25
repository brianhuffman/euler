module Euler193 where
import PrimeArray ( primes_upto )
import Data.Array.Unboxed
import Data.Int ( Int64 )
import Primes

{-
Problem 193
10 May 2008

A positive integer n is called squarefree, if no square of a prime divides n,
thus 1, 2, 3, 5, 6, 7, 10, 11 are squarefree, but not 4, 8, 9, 12.

How many squarefree numbers are there below 2^50?
-}

{-
mobius_array :: Int -> UArray Int Int
mobius_array = multiplicative_array f
  where f (p,e) = if e == 1 then -1 else 0

prob193' :: Int -> Int64
prob193' m = sum
  [ fromIntegral b * (m2 `div` square k) |
    k <- [1 .. m],
    let b = mob ! k ] --,
--    b /= 0 ]
  where
    square :: Int -> Int64
    square n = fromIntegral n ^ 2
    m2 = square m
    mob = mobius_array m
-}

prob193 :: Int -> Int64
prob193 rlim = f 1 ps 0
  where
    ps :: [Int]
    ps = primes_upto rlim

    square :: Int -> Int64
    square n = fromIntegral n ^ 2

    lim :: Int64
    lim = square rlim

    f n [] t = t + lim `div` square n
    f n (p:ps) t
      | p > rlim `div` n = t + lim `div` square n
      | otherwise = f n ps (g (n * p) ps t)

    g n [] t = t - lim `div` square n
    g n (p:ps) t
      | p > rlim `div` n = t - lim `div` square n
      | otherwise = g n ps (f (n * p) ps t)

main :: IO String
main = return $ show $ prob193 (2^25)
-- 684465067343069

{-
Number of squarefree numbers:
624 up to 2^10
2491 up to 2^12
9962 up to 2^14
up to 2^16
up to 2^18
637461 up to 2^20
652756722 up to 2^30
41776432306 up to 2^36
167105729501 up to 2^38
668422917419 up to 2^40
2673691668788 up to 2^42
10694766677567 up to 2^44
42779066708781 up to 2^46
171116266836714 up to 2^48
684465067343069 up to 2^50
-}

{-
All primes are square-free.

2^50 = 1125899906842624 > 10^15.
There is no way I am going to list primes up to this.
I can list primes up to the square root of this.
  2^25 = 33554432  (33 million).
There are 2063689 primes < 2^25.

squarefree <--> prime factorization only has exponents of 1.

Maybe easier to count non-squarefree (squareful) numbers?
All numbers below 2^50 are squarefree except those disqualified
  by some prime p < 2^25.

For primes p between 2^24 and 2^25, the only number first
  disqualified by p is p^2.

For primes p above 2^25 / 3, the only numbers first disqualified
  by p are p^2 * [1,2,3,_,5,6,7,_]

For primes p above 2^25 / 5, the only numbers first disqualified
  by p are p^2 *
  [1,2,3,_,5,6,7,_,_,10,11,_,13,14,15,_,17,_,19,_,21,22,23]
  (i.e. squarefree numbers up to 5^2)

For primes p above 2^25 / 7, the only numbers first disqualified
  by p are p^2 * (squarefree numbers up to 7^2)

...

For primes p above 2^25 / 5791, the only numbers first disqualified
  by p are p^2 * (squarefree numbers up to 5791^2)

5801 is above 2^25 / 5801.

a|b = a + b - ab
a|b|c = a+b+c-ab-ac-bc+abc

How many #s up to 2^50 are disqualified by at least one of
  [2,3,5,7 .. 5791]?  (760 terms)

For each squarefree n < 2^25, with prime factors ps,
of which all primes p have p^2 < 2^25,
calculate x = how many multiples of n^2 are below 2^50
(i.e. 2^50 `div` n^2). If length ps is odd, this counts
positive; if length ps is even, this counts negative.

The total of all xs is the number of #s up to 2^50 disqualified
by at least one of [2,3,5,7 .. 5791].


disqualified by 2: [4, 8, 12, ... 2^50]  (2^48 of them)
disqualified by 3: [9, 18, 27, _, 45, ...]

exponent of 2: [2 .. 50]
exponent of 3: [2 .. 31]
exponent of 5: [2 .. 21]
exponent of 7: [2 .. 17]
exponent of 11: [2 .. 14]
exponent of 13: [2 .. 13]
exponent of 17: [2 .. 12]

2 3 = 6
2 3 5 = 30
2 3 5 7 = 210
2 3 5 7 11 = 2310
2 3 5 7 11 13 = 30030
2 3 5 7 11 13 17 = 510510
2 3 5 7 11 13 17 19 = 9699690
2 3 5 7 11 13 17 19 23 = 223092870

# of squarefree numbers up to 2^50 =
+ # of numbers between 2^25 and 2^50 (2^50 - 2^25)
- # of squareful numbers between 2^25 and 2^50

a squareful number between 2^25 and 2^50

2 -> rules out multiples of 4
3 -> rules out multiples of 9
-}

{-
Number of squarefree numbers:
624 up to 2^10
1245 up to 2^11
2491 up to 2^12
4982 up to 2^13
9962 up to 2^14
637461 up to 2^20
652756722 up to 2^30
41776432306 up to 2^36
167105729501 up to 2^38
668422917419 up to 2^40
2673691668788 up to 2^42
10694766677567 up to 2^44  (~2 min)
42779066708781 up to 2^46  (4-5 min)
171116266836714 up to 2^48
684465067343069 up to 2^50
-}

squarefree :: Integer -> Bool
squarefree n = all (\(p,e) -> e == 1) (prime_factorization n)

--sqrtmax :: Integer
--sqrtmax = 2^5

prob193a :: Int -> (Int64, Int64, Int64)
prob193a sqrtmax = (sumx, sumy, bigmax - sumx - sumy)
  where
    square :: Int -> Int64
    square n = fromIntegral n ^ 2
    bigmax = square sqrtmax
    pfs :: [(Int, [Int])]
    pfs =
      [ (n, ps) |
        n <- [sqrtmax, sqrtmax - 1 .. 1],
        let (ps,es) = unzip (prime_factorization n),
        all (== 1) es ]
    sumx :: Int64
    sumx = sum
      [ (if even (length ps) then - x else x) |
        (n,ps) <- pfs,
        n > 1,
        all (<= pmax) ps,
        let x = bigmax `div` square n ]

    small_squarefree :: [Int]
    small_squarefree = map fst pfs
    all_primes :: [Int]
    all_primes = takeWhile (< sqrtmax) primes
    (small_primes, large_primes) =
      span (\p -> p^2 < sqrtmax) all_primes
    pmax = last small_primes
    -- sumy = # of p <- large_prime, s <- small_squarefree
    -- such that s * p^2 <= bigmax
    sumy :: Int64
    sumy = ok_prods small_squarefree large_primes 0
    -- precondition: xs is reverse-sorted
    ok_prods :: [Int] -> [Int] -> Int64 -> Int64
    ok_prods [] ps l = 0
    ok_prods xs [] l = l * fromIntegral (length xs)
    ok_prods (x:xs) (p:ps) l =
      let t = fromIntegral x * square p
      in if t <= bigmax then ok_prods (x:xs) ps (l+1)
                        else l + ok_prods xs (p:ps) l

prob193b :: Int -> Int64
prob193b sqrtmax = bigmax - totals ns large_primes 0 0
  where
    ns :: [Int]
    ns = [sqrtmax, sqrtmax - 1 .. 1]

    square :: Int -> Int64
    square n = fromIntegral n ^ 2

    bigmax :: Int64
    bigmax = square sqrtmax

    all_primes :: [Int]
    all_primes = takeWhile (< sqrtmax) primes
    (small_primes, large_primes) =
      span (\p -> p^2 < sqrtmax) all_primes

    pmax :: Int
    pmax = last small_primes

    -- totals :: [Int] -> [Int] -> Int64 -> [Int64]
    totals [] ps0 l t = t
    totals (n:ns) ps0 l t =
      if squarefree then (totals ns ps2 y) $! (t+x+y)
                    else totals ns ps0 l t
      where
        (ps,es) = unzip (prime_factorization n)
        squarefree = all (== 1) es
        q = bigmax `div` square n
        x0 = if even (length ps) then -q else q
        x = if n > 1 && all (<= pmax) ps then x0 else 0
        (ps1,ps2) = span (\p -> fromIntegral n * square p <= bigmax) ps0
        y = l + fromIntegral (length ps1)

-- prob193'' :: Int -> Int64
prob193'' rlim = lim - totals 1 primes'
  where
    primes' :: [Int]
    primes' = takeWhile (< rlim) primes

    square :: Int -> Int64
    square n = fromIntegral n ^ 2

    lim :: Int64
    lim = square rlim

    totals :: Int -> [Int] -> Int64
    totals n [] = 0
    totals n (p:ps)
      | rlim < n = 0
      | otherwise = t + totals n ps - totals n' ps
      where
        n' = n * p
        t = lim `div` square n'

prob193d :: Int -> Int64
prob193d rlim = totals [1 .. rlim] 0
  where
    square :: Int -> Int64
    square n = fromIntegral n ^ 2

    lim :: Int64
    lim = square rlim

    -- totals :: [Int] -> Int64 -> [Int64]
    totals [] t = t
    totals (n:ns) t =
      if squarefree then (totals ns) $! (t+x)
                    else totals ns t
      where
        (ps,es) = unzip (prime_factorization n)
        squarefree = all (== 1) es
        q = lim `div` square n
        x = if even (length ps) then q else -q

