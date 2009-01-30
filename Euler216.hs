module Euler216 where
import SquareRoot
import Primes
import PrimeArray
import Data.Int (Int64)
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad.ST
import Control.Monad (foldM)

{-
Problem 216
07 November 2008

Consider numbers t(n) of the form t(n) = 2n^2-1 with n > 1.
The first such numbers are 7, 17, 31, 49, 71, 97, 127 and 161.
It turns out that only 49 = 7*7 and 161 = 7*23 are not prime.
For n <= 10000 there are 2202 numbers t(n) that are prime.

How many numbers t(n) are prime for n <= 50,000,000 ?
-}

{-
For an odd prime p,

p divides 2n^2-1
 <-->
2n^2-1 == 0  (mod p)
 <-->
2n^2 == 1  (mod p)
 <-->
n^2 == 1/2  (mod p)
 -->
1/2 is a quadratic residue mod p
 <-->
2 is a quadratic residue mod p
 <-->
p == 1 or 7  (mod 8)

------------------------------

For prime p == 1 or 7 (mod 8),
p | t(n) <--> n^2 == (p+1)/2 (mod p).

If p | t(n), then
Forall m, p | t(m) <--> m == +/-n (mod p).

-}

type Z = Int64

prob216' :: Z -> Int
prob216' m =
  runST (do
    a <- newArray_ (2, m)
    -- initialize array
    mapM_ (\n -> writeArray a n (t n)) [2 .. m]
    foldM (check a) 0 [2 .. m]
  )
  where
    t :: Z -> Z
    t n = 2*n^2 - 1
    check :: STUArray s Z Z -> Int -> Z -> ST s Int
    check a z n = do
      r <- readArray a n
      if r == 1
        then return z
        else do
          mapM_ (reduce a r) (tail [n, n+r .. m])
          mapM_ (reduce a r) [r-n, 2*r-n .. m]
          if r == t n
            then return (z+1)
            else return z
    reduce :: STUArray s Z Z -> Z -> Z -> ST s ()
    reduce a p n = do
      x <- readArray a n
      let (y, _) = divN x p
      writeArray a n y


{-
If we have a way to calculate modular square roots,
then we could create a decent sieve-like algorithm.
-}

{-
Jacobi rules:
(2m|n) =  (m|n), if n == 1 or 7 (mod 8)
(2m|n) = -(m|n), if n == 3 or 5 (mod 8)
(3|5) = (5|3) = (2|3) = -(1|3) = -1

(m|n) = (n|m), if m == 1 or n == 1 (mod 4)
(m|n) = -(n|m), if m == n == 3 (mod 4)
-}

-- Kronecker symbol (m|n)
kronecker_symbol :: (Integral a) => a -> a -> Int
kronecker_symbol m n
  | m >= 0    = kro m (abs n)
  | otherwise = f4 n (kro (-m) (abs n))
  where
    -- precondition: m, n >= 0
    kro m 0 = if m == 1 then 1 else 0
    kro m n
      | even n = f8 m (kro m (n `div` 2))
      | otherwise = jac m n
    -- precondition: odd n > 0, m >= 0
    jac 0 n = 0
    jac 1 n = 1
    jac m n
      | even m = f8 n (jac (m `div` 2) n)
      | m `mod` 4 == 1 = jac (n `mod` m) m
      | n `mod` 4 == 1 = jac (n `mod` m) m
      | otherwise = - jac (n `mod` m) m
    f8 b x = case b `mod` 8 of
      1 -> x
      3 -> -x
      5 -> -x
      7 -> x
      _ -> 0
    f4 b x = case b `mod` 4 of
      1 -> x
      3 -> -x
      _ -> 0

jacobi_symbol :: (Integral a) => a -> a -> Int
jacobi_symbol m n = kronecker_symbol m n

-- square root of n (mod p)
-- precondition: p is odd prime, n is quadratic residue
shanks_tonelli :: (Integral a) => a -> a -> a
shanks_tonelli n p = loop r0
  where
    (q, s) = divN (p-1) 2
    w = head [ w | w <- [2 ..], jacobi_symbol w p == -1 ]
    v = expMod w q p
    n' = invMod n p
    r0 = expMod n ((q+1)`div`2) p
    loop r = if i == 0 then r else loop r'
      where
        x = (r*r*n') `mod` p
        i = length $ takeWhile (/= 1) $ iterate (\n -> (n*n)`mod`p) x
        r' = (r * expMod v (2^(s-i-1)) p) `mod` p

-- square root of n (mod p)
-- precondition: p is odd prime, n is quadratic residue
sqrtMod_prime :: (Integral a) => a -> a -> a
sqrtMod_prime n p
--  | jacobi_symbol n p == -1 = error "not a quadratic residue"
  | r == 3 = expMod n ((p+1) `div` 4) p
  | r == 1 = shanks_tonelli n p
  where r = p `mod` 4
{-
For prime modulus p == 3 (mod 4),
and n a quadratic residue (mod p),
(n^((p+1)/4))^2
= n^((p+1)/2)
= n^((p-1)/2 + 1)
= n^((p-1)/2) * n
= 1 * n = n
-}

---------------------------------------------------

type I = Int64

prob216_list :: Int -> [Int]
prob216_list m = xs ++ ys
  where
    t :: Int -> Int64
    t n = 2*(fromIntegral n)^2 - 1
    pmax :: Int
    pmax = fromIntegral (square_root (t m))
    pmax2 :: Int
    pmax2 = pmax `div` 2
    a :: UArray Int Bool
    a = odd_prime_array pmax2
    -- primes == 7 or 1 (mod 8), up to pmax
    ps17 :: [Int]
    ps17 = [ 2*n+1 |
             n0 <- [3, 7 .. pmax2-1],
             n <- [n0, n0+1],
             a!n ]
    rs17 :: [(Int, Int)]
    rs17 = [ (p, r) |
             p <- ps17,
             let n = (p+1)`div`2,
             let n' = fromIntegral n :: Int64,
             let p' = fromIntegral p :: Int64,
             let r = fromIntegral (sqrtMod_prime n' p') ]
    updates :: [(Int, Bool)]
    updates =
      [ (n, False) |
        (p, r) <- rs17,
        r' <- [r, p-r],
        n <- [r', p+r' .. m] ]
    b :: UArray Int Bool
    b = accumArray (const id) True (1, m) updates
    -- greatest n such that t(n) <= pmax
    nmax :: Int
    nmax = square_root (pmax2 + 1)
    xs, ys :: [Int]
    xs = [ n | n <- [2 .. nmax], a!(n^2 - 1) ]
    ys = [ n | n <- [nmax+1 .. m], b!n ]

-- prob216 (10^4) = 2202
-- prob216 (10^5) = 17185
-- prob216 (10^6) = 141444
-- prob216 (10^7) = 1200975
-- prob216 (50*10^6) = 5437849

main :: IO String
main = return $ show $ length $ prob216_list (50*10^6)
-- main = return $ show $ prog216 (50*10^6)
-- 5437849

