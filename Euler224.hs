module Euler224 where
import Primes
import qualified SortedList as S
import Control.Monad.ST
import Data.Array.ST
import Data.Array.IO
import Data.Array.IArray
import Data.Int
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (sort)

{-
Problem 224
26 December 2008

Let us call an integer sided triangle with sides a ≤ b ≤ c barely
obtuse if the sides satisfy

a^(2) + b^(2) = c^(2) - 1.

How many barely obtuse triangles are there with perimeter ≤ 75,000,000?
-}



{-
The only solutions (mod 4) to a^2 + b^2 = c^2 - 1
require that a and b are even, and c odd.

c^2 - 1 = (c + 1)(c - 1)

Let a = 2x, b = 2y, and c = 2z+1.

a^2 + b^2 = c^2 - 1.
4x^2 + 4y^2 = 4z^2 + 4z
x^2 + y^2 = z^2 + z
x^2 + y^2 = z(z+1)

The right-hand-side z^2 + z is always even.
Thus x and y are either both even, or both odd.

If x and y are both odd, then z == 1 or 6 (mod 8).
If x and y are both multiples of 4, then z == 0 or 7 (mod 8)
If x and y are both == 2 (mod 4), then z == 0 or 7 (mod 8)
If x and y are different (mod 4), then z == 3 or 4 (mod 8)
If z == 2 or 5 (mod 8), then there will be no solutions.

All solutions have c == 1,3 (mod 8)
All solutions have c == 1,3,9 (mod 16)
All solutions have c == 1,3,9,17,19 (mod 32)
All solutions have c == 1,3,9,17,19,33,35,41,51 (mod 64)
All solutions have c == 0,1,3,6,8 (mod 9)

(a+b+1)(a-b) = aa + ba + a - ab - bb - b
(a+b+1)(a-b) = aa + a - bb - b
(a+b+1)(a-b) = a^2 - b^2 + a - b

a^2 + b^2 = c^2 - 1.
a^2 + b^2 - c^2 + 1 = 0.

x^2 - z = z^2 - y^2

a^2 + b^2 = c^2 - 1
a^2 + 1 = c^2 - b^2
a^2 + 1 = (c - b)(c + b)

a^2 + 1 = r*s
2a <= s - r = 2b
2a+r <= s <= m - a
3a+r <= s+a <= m

It appears that r must be less than m/8. (not a tight bound)

2a^2 = c^2 - 1

(2n, 2n^2, 2n^2+1) is a solution for all n.

5 | n^2 + 1 <--> n == 2,3 (mod 5)

(10n+2, 10n^2+4n-2, 10n^2+4n+3) is a solution for all n.
(10n-2, 10n^2-4n-2, 10n^2-4n+3) is a solution for all n.

a^2 + b^2 = c^2 - 1
a^2 + b^2 = c^2 - 1
a^2 + b^2 = (c+1)(c-1)

a^2 + 1 = (c + b)(c - b)
b^2 + 1 = (c + a)(c - a)


------------------------------
-- Factoring (a^2 + 1).

All factors of (a^2 + 1) are equivalent to 1 (mod 4).

a^2 + 1 is relatively prime to a.

Testing whether a^2 + 1 == 0  (mod p)
If p << a, then we can reduce a (mod p) before squaring.

a^2 + 1 can have at most one prime factor > a.

-}

type Z = Int64

prob224_slow m =
  [ (c,b,a) |
    q <- [0 .. m`div`64],
    r <- [1,3,9,17,19],
    let c = 32*q + r,
    let rhs = c^2 - 1,
    let as = [ (a, a^2) | a <- [2, 4 .. c-1] ],
    let bs = [ (b, b^2) | b <- [c-1, c-3 .. 2] ],
    (a,b) <- sums_to rhs (m-c) as bs,
    a+b+c <= m
  ]

sums_to t _ [] ys = []
sums_to t _ xs [] = []
sums_to t m ((a,x):_) ((b,y):_)
  | y < x = []
  | a + b > m + 1 = []
sums_to t m xs@((a,x):xs') ys@((b,y):ys') =
  case compare (x+y) t of
    LT -> sums_to t m xs' ys
    GT -> sums_to t m xs ys'
    EQ -> (a,b) : sums_to t m xs' ys'

prob224 m =
  [ (a,s) |
    a <- [2,4..m`div`3],
    let a2 = a^2 + 1,
    let pf = factorization ps a2,
    let rs = list_divisors_of_pf pf,
    let rss = [ (r, a2 `div` r) | r <- rs ],
    let rss' = dropWhile (\(r,s) -> m - a < s) rss,
    (r,s) <- takeWhile (\(r,s) -> 2*a <= s - r) rss'
    -- let b = (s - r) `div` 2,
    -- let c = (s + r) `div` 2,
  ]
  where
    ps = [ p | p <- primes, p `mod` 4 == 1 ]

prob224b :: Z -> Int
prob224b m = length (prob224 m)

prob224' m =
  [ (a,b,c) |
    (a,s) <- prob224 m,
    let r = (a^2 + 1) `div` s,
    let b = (s - r) `div` 2,
    let c = (s + r) `div` 2 ]

main :: IO String
main = return $ show $ prob224c (75*10^6)

-- length $ prob224 (10^3) = 55
-- length $ prob224 (10^4) = 543
-- length $ prob224 (10^5) = 5512
-- length $ prob224 (10^6) = 55123    (2:29) (1:39)  (0:01)
-- length $ prob224 (2*10^6) = 110315 (9:14) (6:20)  (0:02)
-- length $ prob224 (4*10^6) = 220557 (35:35) (24:24) (0:05)
-- length $ prob224 (10*10^6) = 551535                (0:15)

-- 4137330 (

---------------------------------------------------------------

-- square root of -1, mod p
sqrt_m1_mod p
  | p `mod` 4 == 1 = try_bases 2
  | otherwise = error "sqrt_m1_mod"
  where
    div2s n k
      | odd n = (n, k)
      | otherwise = div2s (n`div`2) (k+1)
    (d, s) = div2s (p-1) 0
    try_bases b
      | x0 == 1   = try_bases (b+1)
      | x0 == p-1 = try_bases (b+1)
      | otherwise = search x0
      where x0 = expMod b d p
    search x = if x' == p-1 then x else search x'
      where x' = (x * x) `mod` p

sqrt_m1_mod' p = if r1 < r2 then [r1,r2] else [r2,r1]
  where
    r1 = sqrt_m1_mod p
    r2 = p - r1

-- sqrts_m1_mod n = [ a | a <- [1..], a^2 + 1 == 0 (mod n) ]
sqrts_m1_mod 1 = [1 ..]
sqrts_m1_mod p = [ k*p + r | k <- [0..], r <- rs ]
  where
    r1 = sqrt_m1_mod p
    r2 = p - r1
    rs = if r1 < r2 then [r1,r2] else [r2,r1]

good_primes = [ p | p <- primes, p `mod` 4 == 1 ]

good_prime_prods = filter ok [1,5 ..]
  where
    ok n = and [ p `mod` 4 == 1 | (p, e) <- prime_factorization n ]

barely_obtuse :: Z -> [(Z, [(Z, Z, Z)])]
barely_obtuse m = [ (d, triangles (d, pf)) | (d, pf) <- dpfs ]
  where
    sqrt_map = Map.fromList
      [ (p, sqrt_m1_mod' p) |
        p <- takeWhile (< dmax) primes,
        p `mod` 4 == 1 ]
    loop (p,rs) = [ k*p + r | k <- [0..], r <- rs ]
    combine (p1,rs1) (p2,rs2) =
      (p1*p2, sort [ fst (chinese (r1,p1) (r2,p2)) | r1 <- rs1, r2 <- rs2 ])
    dmax = m `div` 8
    dpfs = [ (d, pf) |
             d <- [1, 5 .. m`div`8],
             let pf = prime_factorization d,
             and [ p `mod` 4 == 1 | (p, _) <- pf ]
           ]
    all_sqrts ps = loop
      (foldl1 combine [(p, fromJust (Map.lookup p sqrt_map)) | p <- ps])
    triangles (d, pf) =
      takeWhile (\(a,b,c) -> a+b+c <= m) $
      dropWhile (\(a,b,c) -> b < a) $
      [ (a, b, c) |
        let ps = map fst pf,
        a <- if null ps then [2,4..] else filter even (all_sqrts ps),
        let (s,r) = (a^2 + 1) `divMod` d,
        r == 0,
        let b = (s - d) `div` 2,
        -- 2a + d <= s <= m - a
        let c = b + d
      ]

barely_obtuse2 m = [ (d, length ts) | (d, ts) <- barely_obtuse m ]

foobar = scanl (\(_,_,x) (d,y) -> (d,y,x+y)) (0,0,0) . barely_obtuse2

barely_obtuse_slow :: Z -> [(Z, [(Z, Z, Z)])]
barely_obtuse_slow m = [ (d, triangles (d, pf)) | (d, pf) <- dpfs ]
  where
    sqrt_map = Map.fromList
      [ (p, sqrt_m1_mod' p) |
        p <- takeWhile (< dmax) primes,
        p `mod` 4 == 1 ]
    loop (p,rs) = [ k*p + r | k <- [0..], r <- rs ]
    combine (p1,rs1) (p2,rs2) =
      (p1*p2, [ fst (chinese (r1,p1) (r2,p2)) | r1 <- rs1, r2 <- rs2 ])
{-
    sqrts p = [ k*p + r | k <- [0..], r <- rs ]
      where Just rs = Map.lookup p sqrt_map
-}
    dmax = m `div` 8
    dpfs = [ (d, pf) |
             d <- [1, 5 .. m`div`8],
             let pf = prime_factorization d,
             and [ p `mod` 4 == 1 | (p, _) <- pf ]
           ]
    all_sqrts ps = loop
      (foldl1 combine [(p, fromJust (Map.lookup p sqrt_map)) | p <- ps])
    triangles (d, pf) =
      takeWhile (\(a,b,c) -> a+b+c <= m) $
      dropWhile (\(a,b,c) -> b < a) $
      [ (a, b, c) |
        let ps = map fst pf,
        -- a <- if null ps then [2,4..] else filter even (all_sqrts ps),
        a <- [2,4..],
        let (s,r) = (a^2 + 1) `divMod` d,
        r == 0,
        let b = (s - d) `div` 2,
        -- 2a + d <= s <= m - a
        let c = b + d
      ]

barely_obtuse2_slow m = [ (d, length ts) | (d, ts) <- barely_obtuse_slow m ]

foobar_slow = scanl (\(_,_,x) (d,y) -> (d,y,x+y)) (0,0,0) . barely_obtuse2_slow

prob224c = sum . map (length . snd) . barely_obtuse

barely_obtuse' m =
  [ (d, triangles d) |
    d <- takeWhile (< rmax) good_prime_prods ]
  where
    rmax = m `div` 8
    triangles d =
      takeWhile (\(a,b,c) -> a+b+c <= m) $
      dropWhile (\(a,b,c) -> b < a) $
      [ (a, b, c) |
        a <- filter even (roots_m1 d),
        let s = (a^2 + 1) `div` d,
        let b = (s - d) `div` 2,
        -- 2a + d <= s <= m - a
        let c = b + d
      ]

-- roots_m1 n = [ a | a <- [1..], a^2 + 1 == 0 (mod n) ]
roots_m1 1 = [1 ..]
roots_m1 n = [ k*n + r | k <- [0..], r <- rs ]
  where rs = [ r | r <- [2 .. n-2], (r^2+1) `mod` n == 0 ]

prob224a m =
  [ (a,s) |
    n <- [1 .. m`div`6],
    let a = 2*n,
    let a2 = a^2 + 1,
    let pf = arr ! n,
    let rs = list_divisors_of_pf pf,
    let rss = [ (r, a2 `div` r) | r <- rs ],
    (r,s) <- takeWhile (\(r,s) -> 2*a <= s - r) rss,
    -- let b = (s - r) `div` 2,
    -- let c = (s + r) `div` 2,
    a + s <= m
  ]
  where
    arr = square_plus_one_factor_array (m`div`2)

square_plus_one_factor_array :: Integer -> Array Integer [(Integer, Int)]
square_plus_one_factor_array m =
  runSTArray (do
    a <- newArray (1, m) (1 :: Integer)
    mapM_ (fill a) [1 .. m]
    b <- newArray (1, m) ([] :: [(Integer, Int)])
    mapM_ (check a b) [1 .. m]
    return b
  )
  where
    fill :: STArray s Integer Integer -> Integer -> ST s ()
    fill a n = writeArray a n (4*n^2 + 1)
    check :: STArray s Integer Integer -> STArray s Integer [(Integer, Int)] -> Integer -> ST s ()
    check a b n = do
      k <- readArray a n
--      (if k > 1 then reduce_all a b n k else return ())
      let ps = map fst (factorization good_primes k)
      mapM_ (reduce_all a b n) ps
    reduce_all :: STArray s Integer Integer -> STArray s Integer [(Integer, Int)] -> Integer -> Integer -> ST s ()
    reduce_all a b n p =
      mapM_ (reduce a b p) [n, n+p .. m]
    reduce :: STArray s Integer Integer -> STArray s Integer [(Integer, Int)] -> Integer -> Integer -> ST s ()
    reduce a b p n = do
      k <- readArray a n
      let (r, i) = divN k p
      writeArray a n r
      pf <- readArray b n
      writeArray b n ((p, i) : pf)


square_plus_one_factor_array' :: Integer -> IO ()
square_plus_one_factor_array' m =
  do
    a <- newArray (1, m) (1 :: Integer)
    mapM_ (fill a) [1 .. m]
    b <- newArray (1, m) ([] :: [(Integer, Int)])
    mapM_ (check a b) [1 .. m]
    return ()
  where
    fill :: IOArray Integer Integer -> Integer -> IO ()
    fill a n = writeArray a n (n^2 + 1)
    check :: IOArray Integer Integer -> IOArray Integer [(Integer, Int)] -> Integer -> IO ()
    check a b n = do
      k <- readArray a n
      (if k > 1 then reduce_all a b n k else return ())
{-
      let pf = prime_factorization k
      (if length pf > 1 then print (n,k,pf) else return ())
      mapM_ (reduce_all a b n) pf
-}
    reduce_all a b n p =
      mapM_ (reduce a b p) [n, n+p .. m]
    reduce a b p n = do
      k <- readArray a n
      let (r, i) = divN k p
      writeArray a n r
      pf <- readArray b n
      writeArray b n ((p, i) : pf)
