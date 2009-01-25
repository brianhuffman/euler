module Euler211 where
import PrimeArray
import Primes
import EulerLib (square_root)
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Int ( Int64 )

{-

Problem 211
04 October 2008

For a positive integer n, let σ_(2)(n) be the sum of the squares of
its divisors. For example,

σ_(2)(10) = 1 + 4 + 25 + 100 = 130.

Find the sum of all n, 0 < n < 64,000,000 such that σ_(2)(n) is a
perfect square.

-}

type Z = Integer
type N = Int64


--------------------------------------------------
-- Testing for squares

qr_array :: Int -> UArray Int Bool
qr_array m = accumArray (||) False (0, m-1)
  [ (n^2 `mod` m, True) | n <- [0 .. m `div` 2] ]

qr256 :: UArray Int Bool
qr256 = qr_array 256

qr255 :: UArray Int Bool
qr255 = qr_array 255

qr1001 :: UArray Int Bool
qr1001 = qr_array 1001

is_square :: N -> Bool
is_square n =
  qr256 ! (fromIntegral (n `mod` 256)) &&
  qr255 ! (fromIntegral (n `mod` 255)) &&
  qr1001 ! (fromIntegral (n `mod` 1001)) &&
  (square_root n)^2 == n

--------------------------------------------------

sigma2_array :: N -> UArray N N
sigma2_array m =
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
    f :: (N, N) -> N
    f (p, e) = g e
      where
        p2 = fromIntegral p ^ 2
        g 0 = 1
        g e = 1 + p2 * g (e-1)
    check :: STUArray s N N -> N -> ST s ()
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
    divN :: N -> N -> (N, N)
    divN x p
      | r == 0 = let (z, n) = divN q p in (z, n+1)
      | otherwise = (x, 0)
      where (q, r) = divMod x p

sigma2_squares :: N -> [N]
sigma2_squares m = [ n | n <- [1 .. m], is_square (s2 ! n) ]
  where s2 = sigma2_array m

main :: IO String
main = return $ show $ sum $ sigma2_squares (64*10^6)

answer :: String
answer = "1922364685"
