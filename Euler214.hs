module Euler214 where
import Primes (totient)
import Data.Array.ST
import Data.Int (Int8)
import Control.Monad.ST
import Data.STRef

{-
Problem 214
Totient Chains

25 October 2008

Let φ be Euler's totient function, i.e. for a natural number n, φ(n)
is the number of k, 1 <= k <= n, for which gcd(k,n) = 1.

By iterating φ, each positive integer generates a decreasing chain of
numbers ending in 1. E.g. if we start with 5 the sequence 5,4,2,1 is
generated.

Here is a listing of all chains with length 4:
5,4,2,1
7,6,2,1
8,4,2,1
9,6,2,1
10,4,2,1
12,4,2,1
14,6,2,1
18,6,2,1

Only two of these chains start with a prime, their sum is 12.

What is the sum of all primes less than 40 million which generate a
chain of length 25?
-}

{-
For all n, phi(2n) <= n.

If 2^k+1 is prime, then length (totient_chain (2^k+1)) = k+2.

All other totient chains of the same length have higher start values.

Least possible start value for length l = 2^(l-2) + 1.

phi(2n) = phi(n), if n is odd
phi(2n) = 2 phi(n), if n is even

Define t(n) = length of totient chain starting with n.

t(1) = 1
t(2) = 2

t(2n) = t(n), for odd n.
t(2n) = t(n) + 1, for even n.
t(n * 2^k) = t(n) + (k-1), for odd n.

t(m*n) = t(m) + t(n) - 2, for odd n > 1.
t(p) = t(p-1) + 1, for prime p.

Using these rules, it is possible to generate a table of chain lengths
directly, without calculating the totients themselves.

-}

prob214 :: Int -> Int8 -> Integer
prob214 m l =
  runST (do
    a <- newArray (1, m) 2
    s <- newSTRef 0
    mapM_ (check a s) [3, 5 .. m]
    readSTRef s
  )
  where
    check a s n = do
      t <- readArray a n
      if t == 2 then do_prime a s n else return ()
    do_prime a s p = do
      let (q,k) = div2s (p-1) 0 -- q*2^k = p-1
      tq <- readArray a q -- (p-1)
      let tp = tq + k
      mapM_ (add_many a (tp-2)) (powers p p)
      if tp == l then (do z <- readSTRef s; writeSTRef s (z + toInteger p))
                 else (return ())
    add :: STUArray s Int Int8 -> Int8 -> Int -> ST s ()
    add a x i = do
      y <- readArray a i
      writeArray a i (x+y)
    add_many a x i = mapM_ (add a x) [i, 2*i .. m]
    powers p n
      | n <= m `div` p = n : powers p (p*n)
      | otherwise = [n]
    div2s n k
      | odd n = (n, k)
      | otherwise = div2s (n`div`2) (k+1)

totient_chain :: Integer -> [Integer]
totient_chain 1 = [1]
totient_chain n = n : totient_chain (totient n)

main :: IO String
main = return $ show $ prob214 (40*10^6) 25 

answer :: String
answer = "1677366278943"
