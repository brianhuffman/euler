{-# LANGUAGE FlexibleContexts #-}

module PrimeArray where
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

-------------------------------------------------------------------

prime_array :: Int -> UArray Int Bool
prime_array m =
  runSTUArray (do
    a <- newArray (1, m) True
    mapM_ (check a) (takeWhile (\n -> n*n <= m) [2 ..])
    return a
  )
  where
    check a n = do
      p <- readArray a n
      if p then mapM_ (clear a) [n^2, n^2+n .. m]
           else return ()
    clear a n = writeArray a n False
-- almost 10 sec to generate primes up to 10^8

-- odd_prime_array m ! n = is_prime (2*n+1)
odd_prime_array :: Int -> UArray Int Bool
odd_prime_array m =
  runSTUArray (do
    a <- newArray (1, m) True
    mapM_ (check a) (takeWhile (\n -> 2*n*(n+1) <= m) [1 ..])
    return a
  )
  where
    check a n = do
      p <- readArray a n
      let k = 2*n+1
      let clear n = writeArray a n False
      if p then mapM_ clear [2*n*(n+1), 2*n*(n+1)+k .. m]
           else return ()
-- about 4.5 sec to generate primes up to 10^8

{-
address n represents p = (2n+1)
(2n+1)^2 = (4n^2 + 4n + 1) = 2(2n^2+2n) + 1
p^2 is found at address (2n^2 + 2n).
p^2 + 2p is found at address (2n^2 + 4n + 1).
-}

primes_upto :: Int -> [Int]
primes_upto m = 2 : [ 2*n+1 | n <- [1 .. m'], a!n ]
  where
    m' = (m-1) `div` 2
    a = odd_prime_array m'

-------------------------------------------------------------------
-- multiplicative functions

multiplicative_array :: ((Int, Int) -> Int) -> Int -> UArray Int Int
multiplicative_array f m =
  runSTUArray (do
    a <- newArray (1, m) 0
    -- a[n] <- a prime factor of n
    mapM_ (check a) (takeWhile (\n -> n*n <= m) [2 ..])
    -- multiplicative functions must map 1 to 1
    writeArray a 1 1
    -- a[n] <- function evaluated at n
    mapM_ (eval a) [2 .. m]
    return a
  )
  where
    check a n = do
      d <- readArray a n
      if d /= 0 then return () else do
      mapM_ (\k -> writeArray a k n) [n^2, n^2+n .. m]
    eval a n = do
      p <- readArray a n  -- get prime divisor of n
      if p == 0 then writeArray a n (f (n, 1)) else do
      let (r, e) = divN n p  -- n = r * p^e
      if r == 1 then writeArray a n (f (p, e)) else do
      x <- readArray a r
      y <- readArray a (p^e)
      writeArray a n (x*y)
    divN :: Int -> Int -> (Int, Int)
    divN x p
      | r == 0 = let (z, n) = divN q p in (z, n+1)
      | otherwise = (x, 0)
      where (q, r) = divMod x p

num_divisors_array :: Int -> UArray Int Int
num_divisors_array = multiplicative_array f
  where f (p,e) = e+1

sum_divisors_array :: Int -> UArray Int Int
sum_divisors_array = multiplicative_array f
  where f (p,e) = sum [ p^k | k <- [0 .. e] ]

-------------------------------------------------------------------

{-
num_divisors_array :: Int -> UArray Int Int
num_divisors_array m = accumArray (+) 2 (2, m) updates
  where
    ds = takeWhile (\d -> d^2 <= m) [2 ..]
    f d = (d*d, 1) : [ (x, 2) | x <- [d*(d+1), d*(d+2) .. m] ]
    updates = concatMap f ds

sum_divisors_array :: Int -> UArray Int Int
sum_divisors_array m = accumArray (+) 0 (1, m) updates
  where
    ds = takeWhile (\d -> d^2 <= m) [1 ..]
    f x = (x^2, x) : [ (x*y, x+y) | y <- [x+1 .. m `div` x] ]
    updates = concatMap f ds
-}

-------------------------------------------------------------------

{-
largest_prime_factor :: Int -> UArray Int Int
largest_prime_factor m = accumArray (const id) 1 (1, m) updates
  where
    ps = takeWhile (<= m) primes
    updates = [ (n, p) | p <- ps, n <- [p, 2*p .. m] ]
-}

-------------------------------------------------------------------

totient_array :: Int -> UArray Int Int
totient_array m =
  runSTUArray (do
    a <- newArray (1, m) 1
    mapM_ (check a) [2 .. m]
    return a
  )
  where
    check a n = do
      t <- readArray a n
      if t == 1 then do_prime a n else return ()
    do_prime a p = do
      mult_many a (p-1) p
      mapM_ (mult_many a p) (powers p p)
    mult a x i = do
      y <- readArray a i
      writeArray a i (x*y)
    mult_many a x i = mapM_ (mult a x) [i, 2*i .. m]
    powers p n
      | n <= m `div` p = p' : powers p p'
      | otherwise = []
      where p' = p*n
-- 18 sec to generate totients up to 40 million

{-
totient_array' :: Int -> UArray Int Int
totient_array' m =
  runSTUArray (do
    a <- newArray_ (1, m)
    writeArray a 1 1
    mapM_ (tot a) [2 .. m]
    return a
  )
  where
    tot a n = do
      let p = least_prime_divisor n
      let (q, e) = divN n p
      x <- readArray a q
      writeArray a n ((p-1) * p^(e-1) * x)
-}

{-
Array writes for num_divisors array:
  sum [ n`div`k | k <- [1 .. n] ]
10^2:  482
10^3:  7069
10^4:  93668
10^5: 1166750
10^6: 13970034
 O(n log n).
Starting writes at k^2 speeds up by factor of 2.

Array writes for primes array:
  sum [ n`div`p | p <- takeWhile (<=n) primes ]
10^2: 171
10^3: 2126
10^4: 24300
10^5: 266400
10^6: 2853708
  sum [ n`div`p | p <- takeWhile (\p -> p^2<=n) primes ]
10^2: 117
10^3: 1560
10^4: 18016
10^5: 202219
10^6: 2198007

ratios:
10^2: 2.818
10^3: 3.325
10^4: 3.854
10^5: 4.379
10^6: 4.895

-}
