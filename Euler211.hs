module Euler211 where
import PrimeArray
import Primes
import SquareRoot
import ContinuedFraction
import Permutation (subsets)
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Int ( Int64 )
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified SortedList as S

{-

Problem 211
04 October 2008

For a positive integer n, let σ_(2)(n) be the sum of the squares of
its divisors. For example,

σ_(2)(10) = 1 + 4 + 25 + 100 = 130.

Find the sum of all n, 0 < n < 64,000,000 such that σ_(2)(n) is a
perfect square.

-}

{-
Define m = 64 million.
Define r = 8000 = sqrt(m).

For all primes p <= r, calculate s2(p^e) for powers p^e <= m.
Calculate the square-free part t(p,e) of each s2(p^e).

Any solution n such that s2(n) is a perfect square, either
1) has all prime power factors below r, or
2) has a single prime power factor larger than r, or
3) has a single prime factor larger than r.

In case 1, we can use the table of t(p,e) to build up products that
are perfect squares (i.e. have square-free part = 1)

In case 2, we can factor s2(p^e) for each p <= r, and e such that r <=
p^e <= m. We do trial division, only testing factors that occur in the
table of t(p,e).

In case 3, we can use the table of t(p,e) to build up a product with a
square-free part = D, then search for large primes p such that p^2 + 1
= D*k^2, for some k.

Also, for any odd prime divisor q of D,
p^2 + 1 == 0  (mod q)
p^2 == -1  (mod q)
-1 is quadratic residue mod q
q == 1  (mod 4)

Quadratic residues mod 8 are 0, 1, 4.
For odd prime p,
p^2 == 1 (mod 8)
p^2 + 1 == 2 (mod 8)
D*k^2 == 2 (mod 8)
D == 2 (mod 8) and k is odd.

Solutions only exist for D == 2 (mod 8).

D = 10 = 2 5: 3/1, 117/37, 4443/1405, 168717/53353, ...
D = 18 = 2 3 3: none
D = 26 = 2 13: 5/1, 515/101, 52525/10301, ...
D = 34 = 2 17: none
D = 42 = 2 3 7: none
D = 50 = 2 5 5: 7/1, 1393/197, 275807/39005, ...
D = 58 = 2 29: 99/13, 3881493/509665, ...
D = 66 = 2 3 11: none
D = 74 = 2 37: 43/5, 318157/36985, ...
D = 82 = 2 41: 9/1, 2943/325, 959409/105949, ...
D = 90 = 2 3 3 5: none
-}

type Z = Integer
type N = Int64

----------------------------------------------------
-- Sigma2

-- (n, descending prime factors of squarefree part of s2(n))
data Sigma2 = Sigma2 Z [Z]

mergeSigma2 :: Sigma2 -> Sigma2 -> Sigma2
mergeSigma2 (Sigma2 x ps) (Sigma2 y qs) = Sigma2 (x*y) (merge ps qs)
  where
    merge xs [] = xs
    merge [] ys = ys
    merge xs@(x:xs') ys@(y:ys') =
      case compare x y of
        GT -> x : merge xs' ys
        LT -> y : merge xs ys'
        EQ -> merge xs' ys'

mergeAllSigma2 :: [Sigma2] -> Sigma2
mergeAllSigma2 = foldl mergeSigma2 (Sigma2 1 [])

headSigma2 :: Sigma2 -> Z
headSigma2 (Sigma2 _ (d:ds)) = d
headSigma2 (Sigma2 _ []) = 1

fstSigma2 :: Sigma2 -> Z
fstSigma2 (Sigma2 x _) = x

nullSigma2 :: Sigma2 -> Bool
nullSigma2 (Sigma2 _ ps) = null ps

coprimeSigma2 :: Sigma2 -> Sigma2 -> Bool
coprimeSigma2 (Sigma2 x _) (Sigma2 y _) = gcd x y == 1

makeSigma2 :: (Z, Int) -> Sigma2
makeSigma2 (p, e) = Sigma2 (p^e) (reverse qs)
  where
    s2 = s2_prime_power (p, e)
    qs = [ q | (q, k) <- prime_factorization s2, odd k ]

emptySigma2 :: Sigma2
emptySigma2 = Sigma2 1 []

----------------------------------------------------

s2_table1 :: Z -> [(Z, [Sigma2])]
s2_table1 m = reverse (Map.toAscList map1)
  where
    r = square_root m
    ps = takeWhile (<=r) primes
    sigma2s = [ makeSigma2 (p, e) | p <- ps, e <- [1 .. ilog p r] ]
    map1 = Map.fromListWith (++) [ (headSigma2 x, [x]) | x <- sigma2s ]

find1 :: Z -> Sigma2 -> [(Z, [Sigma2])] -> [Z]
find1 m x [] = if nullSigma2 x then [fstSigma2 x] else []
find1 m x ((q,ys):rest) =
  [ z |
    headSigma2 x <= q,
    y <- map mergeAllSigma2 (subsets ys),
    coprimeSigma2 x y,
    let x' = mergeSigma2 x y,
    fstSigma2 x' <= m,
    z <- find1 m x' rest ]

s2_squares' :: Z -> ([Z], [Z], [Z])
s2_squares' m = (case1, case2, case3)
  where
    r :: Z
    r = square_root m

    ps :: [Z]
    ps = takeWhile (<=r) primes

    table1 :: [(Z, [Sigma2])]
    table1 = s2_table1 m

    small_qs :: [Z]
    small_qs = reverse (map fst table1)

    table2 :: [Sigma2]
    table2 = [ Sigma2 (p^e) (reverse qs) |
      p <- ps,
      e <- [ilog p r + 1 .. ilog p m],
      let s2 = s2_prime_power (p, e),
      Just qs <- [sfree small_qs s2] ]

    find3 :: Sigma2 -> [(Z, [Sigma2])] -> [(Z, Z)]
    find3 (Sigma2 x qs) []
      | null qs = []
      | last qs /= 2 = []
      | any (\p -> p `mod` 4 /= 1) (init qs) = []
      | otherwise = [(x, product qs)]
    find3 x ((_,ys):rest) =
      [ z |
        y <- map mergeAllSigma2 (subsets ys),
        coprimeSigma2 x y,
        let x' = mergeSigma2 x y,
        fstSigma2 x' <= r,
        z <- find3 x' rest ]

    table3 :: [(Z, Z)]
    table3 = find3 emptySigma2 table1

    -- solutions with all prime power factors below r
    case1 :: [Z]
    case1 = find1 m emptySigma2 table1

    -- solutions with a single prime power factor larger than r
    case2 :: [Z]
    case2 = concat [ find1 m x table1 | x <- table2 ]
    -- solutions with a single prime factor larger than r
    case3 :: [Z]
    case3 =
      [ x * p |
        (x, d) <- table3,
        let amax = m `div` x,
        let cs0 = convergents (sqrt_cfraction d),
        let cs1 = dropWhile ((<=r) . fst) cs0,
        let cs2 = takeWhile ((<=amax) . fst) cs1,
        (p, k) <- cs2,
        p^2 + 1 == d * k^2,
        is_prime p ]

----------------------------------------------------

-- integer logarithm
-- ilog b n = greatest e such that b^e <= n
ilog :: (Integral a) => a -> a -> Int
ilog b n
  | n < b = 0
  | otherwise = 1 + ilog b (n `div` b)

s2_prime_power :: (Z, Int) -> Z
s2_prime_power (p, e) = f e
  where
    p2 = p^2
    f 0 = 1
    f e = 1 + p2 * f (e-1)

-- [(e, squarefree factors of s2(p^e))]
table :: Z -> Z -> [(Int, [Z])]
table r p =
  [ (e, qs) |
    e <- [0 .. ilog p r],
    let s2 = s2_prime_power (p, e),
    let qs = [ q | (q, k) <- prime_factorization s2, odd k ] ]

-- prime factors of squarefree part of n
-- post-condition: must be subset of qs
sfree :: [Z] -> Z -> Maybe [Z]
sfree [] n = if is_square n then Just [] else Nothing
sfree (q:qs) n
  | q > n = sfree [] n
  | otherwise = fmap (\ds -> if odd e then q:ds else ds) (sfree qs n')
  where (n', e) = divN n q

s2_squares :: Z -> ([Z], [Z], [Z])
s2_squares m = (case1, case2, case3)
  where
    r :: Z
    r = square_root m

    ps :: [Z]
    ps = takeWhile (<=r) primes

    table1 :: [(Z, [(Int, [Z])])]
    table1 = reverse [ (p, table r p) | p <- ps ]

    small_qs :: [Z]
    qss :: [[Z]]
    (small_qs : qss) = scanr S.union []
      (map (foldl1 S.union . map snd . snd) table1)

    find1 :: Z -> [Z] -> [((Z, [(Int, [Z])]), [Z])] -> [Z]
    find1 x qs [] = [x]
    find1 x qs (((p,eds),fs):rest) =
      [ z |
        (e, ds) <- eds,
        x <= m `div` (p^e),
        let x' = x * p^e,
        let qs' = merge_factors qs ds,
        subset qs' fs,
        z <- find1 x' qs' rest ]

    table2 :: [((Z, Int), [Z])]
    table2 = [ ((p, e), qs) |
      p <- ps,
      e <- [ilog p r + 1 .. ilog p m],
      let s2 = s2_prime_power (p, e),
      Just qs <- [sfree small_qs s2] ]

    find3 :: Z -> [Z] -> [(Z, [(Int, [Z])])] -> [(Z, Z)]
    find3 x qs []
      | take 1 qs /= [2] = []
      | any (\p -> p `mod` 4 /= 1) (tail qs) = []
      | otherwise = [(x, product qs)]
    find3 x qs ((p,eds):rest) =
      [ z |
        (e, ds) <- eds,
        x <= r `div` (p^e),
        let x' = x * p^e,
        let qs' = merge_factors qs ds,
        z <- find3 x' qs' rest ]

    table3 :: [(Z, Z)]
    table3 = find3 1 [] table1

    -- solutions with all prime power factors below r
    case1 :: [Z]
    case1 = find1 1 [] (zip table1 qss)

    -- solutions with a single prime power factor larger than r
    case2 :: [Z]
    case2 = concat
      [ find1 (p^e) fs (zip table1 qss) | ((p,e),fs) <- table2 ]

    -- solutions with a single prime factor larger than r
    case3 :: [Z]
    case3 =
      [ x * p |
        (x, d) <- table3,
        let amax = m `div` x,
        let cs0 = convergents (sqrt_cfraction d),
        let cs1 = dropWhile ((<=r) . fst) cs0,
        let cs2 = takeWhile ((<=amax) . fst) cs1,
        (p, k) <- cs2,
        p^2 + 1 == d * k^2,
        is_prime p ]

prob211 :: Z -> Z
prob211 m = sum xs + sum ys + sum zs
  where (xs, ys, zs) = s2_squares' m

subset :: [Z] -> [Z] -> Bool
subset [] ys = True
subset xs [] = False
subset xs@(x:xs') ys@(y:ys') =
  case compare x y of
    LT -> False
    GT -> subset xs ys'
    EQ -> subset xs' ys'

merge_factors' :: [Z] -> [Z] -> [Z]
merge_factors' xs [] = xs
merge_factors' [] ys = ys
merge_factors' xs@(x:xs') ys@(y:ys') =
  case compare x y of
    GT -> x : merge_factors' xs' ys
    LT -> y : merge_factors' xs ys'
    EQ -> merge_factors' xs' ys'

merge_factors :: [Z] -> [Z] -> [Z]
merge_factors xs [] = xs
merge_factors [] ys = ys
merge_factors xs@(x:xs') ys@(y:ys') =
  case compare x y of
    LT -> x : merge_factors xs' ys
    GT -> y : merge_factors xs ys'
    EQ -> merge_factors xs' ys'


{-
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
-}

main :: IO String
main = return $ show $ prob211 (64*10^6)

answer :: String
answer = "1922364685"
