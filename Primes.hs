module Primes where
import qualified SortedList as S
import Data.Array.Unboxed

-- coprime m n = gcd m n == 1

divides_none [] n = True
divides_none (p:ps) n
  | n < p*p        = True
  | n `mod` p == 0 = False
  | otherwise      = divides_none ps n

primesInt :: [Int]
primesInt = [2,3,5,7,11,13,17,19,23,29] ++ filter p (f 30)
  where
    p n = divides_none primesInt n
    f n = (n+1):(n+7):(n+11):(n+13):(n+17):(n+19):(n+23):(n+29):f(n+30)

primes :: (Num a) => [a]
primes = map fromIntegral primesInt

least_prime_divisor n = f primes
  where
    f (p : ps)
      | p * p > n      = n
      | n `mod` p == 0 = p
      | otherwise      = f ps

is_prime n
  | n < 2 = False
  | otherwise = divides_none primes n

prime_table :: (Integral a, Ix a) => (a, a) -> UArray a Bool
prime_table (i,j) = accumArray (&&) True (i,j) xs
  where
    ps = takeWhile (\p -> p^2 <= j) primes
    xs = [ (n, False) |
           p <- ps,
           let k = max p ((i+p-1) `div` p),
           n <- [p*k, p*k+p .. j] ]

--------------------------
-- Modular Arithmetic

-- (a^b) `mod` n
expMod a b n
  | b == 0 = 1 `mod` n
  | b == 1 = a `mod` n
  | otherwise = seq x (if even b then x else (a * x) `mod` n)
      where x = expMod ((a * a) `mod` n) (b `div` 2) n

-- extended_gcd a b = (u, v, g)
-- such that a*u + b*v = g = gcd(a,b)
extended_gcd a b = f a b 0 1 1 0
  where
    f a 0 x y x' y' = (x',y',a)
    f a b x y x' y' = f b r (x' - q*x) (y' - q*y) x y
      where (q,r) = a `divMod` b

-- (a^-1) `mod` n
invMod a n = if g /= 1 then error "invMod" else u'
  where
    a' = a `mod` n
    (u,v,g) = extended_gcd a' n
    u' = u `mod` n

chinese (x,a) (y,b)
  | g == 1 = ((x*b*v + y*a*u) `mod` c, c)
  | otherwise = error "chinese: not coprime"
  where
    (u,v,g) = extended_gcd a b
    c = a * b

---------------------
-- Primality tests --

-- Fermat primality test
fermat_pseudoprime_base a n =
  expMod a (n-1) n == 1

fermat_pseudoprime n =
  fermat_pseudoprime_base 2 n &&
  fermat_pseudoprime_base 3 n

-- Miller-Rabin primality test
miller_rabin_base :: Integer -> Integer -> Bool
miller_rabin_base a n = n <= a || x0 == 1 || f x0 s
  where
    (d, s) = div2s (n-1) 0
    x0 = expMod a d n
    f x 0 = False
    f x i = let x' = (x * x) `mod` n
            in x == n-1 || f x' (i-1)
    div2s n k
      | odd n = (n, k)
      | otherwise = div2s (n`div`2) (k+1)

-- See http://primes.utm.edu/prove/prove2_3.html
miller_rabin :: Integer -> Bool
miller_rabin n
  | n <= 2 = n == 2
  | even n = False
  | n < 10000 = is_prime n
  | any (\p -> n `mod` p == 0) (takeWhile (<100) primes) = False
  | n < 1373653    = all (\a -> miller_rabin_base a n) [2,3]
  | n < 4759123141 = all (\a -> miller_rabin_base a n) [2,7,61]
  | n < 341550071728321 = all (\a -> miller_rabin_base a n) [2,3,5,7,11,13,17]
  | otherwise = error ("miller_rabin: argument too big: " ++ show n)

{- large primes: [100907200001, 106907803649, 140800000001, 165888000001, 170774437501, 182521213001, 187500000001, 212733001729, 249832960001, 266368000001, 275200000001, 289910292481, 292968750001, 328125000001, 384000000001, 390625000001, 395640832001, 402653184001, 409600000001, 469620340001, 498018906251, 506159104001, 522343750001, 549316406251, 553648128001, 604991362501, 630240900001, 671875000001, 720000000001, 799744000001, 851968000001, 864000000001, 947147262401] -}

--------------------------
-- Multiplicative order --

-- multiplicative order of a (mod m)
-- i.e. least k such that a^k == 1 (mod m)
mult_order a m
  | gcd a m /= 1 = 0 -- arguments not coprime
  | otherwise    = foldl1 lcm $ map (mult_order' a) $ prime_factorization m

mult_order' a (p,k) = r where
  pk = p^k
  t = (p-1)*p^(k-1) -- totient \Phi(p^k)
  r = product $ map find_qd $ prime_factorization $ t
  find_qd (q,e) = q^d where
    x = expMod a (t `div` (q^e)) pk
    d = length $ takeWhile (/= 1) $ iterate (\y -> expMod y q pk) x

{-
prime_sieve m = f a0 2
  where
    f a p
      | p*p < m = f (sieve a p) (nextp a (p+1))
      | otherwise = a
    a0 = accumArray (const id) True (1,m) [(1,False)]
    sieve a p = accum (const id) a
      (zip (takeWhile (<=m) [p*p,p*p+p..]) (repeat False))
    nextp a p = if a!p then p else nextp a (p+1)
-}

--------------------------
-- Prime Factorizations --

divN :: (Integral a) => a -> a -> (a, Int)
divN x p
  | r == 0 = let (z, n) = divN q p in (z, n+1)
  | otherwise = (x, 0)
  where (q, r) = divMod x p

exponent_of :: (Integral a) => a -> a -> Int
exponent_of p x = if r == 0 then exponent_of p q + 1 else 0
  where (q, r) = divMod x p

factorization [] x = [(x, 0)]
factorization (p:ps) x
  | x == 1 = []
  | x < p^2 = [(x, 1)]
  | otherwise =
      case divN x p of
        (_, 0) -> factorization ps x
        (y, e) -> (p, e) : factorization ps y

prime_factorization n = factorization primes n

num_divisors' m n =
  product $
  map (1+) $
  map (flip exponent_of n) $
  takeWhile (<m) primes

list_divisors_of_pf [] = [1]
list_divisors_of_pf ((p,e):pes) =
  let fs = list_divisors_of_pf pes in
    foldl1 S.union $
    map (\n -> map ((p^n)*) fs) [0 .. e]

list_divisors n = list_divisors_of_pf (prime_factorization n)

sum_of_divisors n = product $ map f $ prime_factorization n
  where
    f (p,1) = 1 + p
    f (p,e) = 1 + p * f (p, e-1)

num_divisors_of_pf = product . map (1+) . map snd

num_divisors n = num_divisors_of_pf (prime_factorization n)

totient n = product (map f (prime_factorization n))
  where f (p,e) = (p-1) * p^(e-1)
