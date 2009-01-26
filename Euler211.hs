module Euler211 where
import PrimeArray
import Primes
import EulerLib (square_root)
import ContinuedFraction
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Int ( Int64 )
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

type Z = Integer
type N = Int64

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

s2_squares :: Z -> ([Z], [Z], [Z])
s2_squares m = (case1, case2, case3)
  where
    r :: Z
    r = square_root m
    ps :: [Z]
    ps = takeWhile (<=r) primes
    table :: Z -> [(Int, [Z])]
    table p = [ (e, qs) |
      e <- [0 .. ilog p r],
      let s2 = s2_prime_power (p, e),
      let qs = [ q | (q, k) <- prime_factorization s2, odd k ] ]
    table1 :: [(Z, [(Int, [Z])])]
    table1 = reverse [ (p, table p) | p <- ps ]
    (small_qs : qss) = scanr S.union []
      (map (foldl1 S.union . map snd . snd) table1)
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
      p <- takeWhile (<=r) primes,
      e <- [ilog p r + 1 .. ilog p m],
      let s2 = s2_prime_power (p, e),
      Just qs <- [sfree small_qs s2] ]
    sfree :: [Z] -> Z -> Maybe [Z]
    sfree [] n = if is_square n then Just [] else Nothing
    sfree (q:qs) n
      | q > n = sfree [] n
      | otherwise = fmap (\ds -> if odd e then q:ds else ds) (sfree qs n')
      where (n', e) = divN n q
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
    table3 = find3 1 [] table1
    -- solutions with all prime power factors below r
    case1 = find1 1 [] (zip table1 qss)
    -- solutions with a single prime power factor larger than r
    case2 = concat
      [ find1 (p^e) fs (zip table1 qss) | ((p,e),fs) <- table2 ]
    -- solutions with a single prime factor larger than r
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
  where (xs, ys, zs) = s2_squares m

subset :: [Z] -> [Z] -> Bool
subset [] ys = True
subset xs [] = False
subset xs@(x:xs') ys@(y:ys') =
  case compare x y of
    LT -> False
    GT -> subset xs ys'
    EQ -> subset xs' ys'

merge_factors :: [Z] -> [Z] -> [Z]
merge_factors xs [] = xs
merge_factors [] ys = ys
merge_factors xs@(x:xs') ys@(y:ys') =
  case compare x y of
    LT -> x : merge_factors xs' ys
    GT -> y : merge_factors xs ys'
    EQ -> merge_factors xs' ys'


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

-- is_square :: N -> Bool
is_square n =
  qr256 ! (fromIntegral (n `mod` 256)) &&
  qr255 ! (fromIntegral (n `mod` 255)) &&
  qr1001 ! (fromIntegral (n `mod` 1001)) &&
  (square_root n)^2 == n

--------------------------------------------------
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
