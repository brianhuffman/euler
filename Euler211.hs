module Euler211 where
import Primes
import EulerLib
import SortedList (unionOf)
import List

{-
Problem 211
04 October 2008

For a positive integer n, let σ2(n) be the sum of the squares of
its divisors. For example, σ2(10) = 1 + 4 + 25 + 100 = 130.

Find the sum of all n, 0<n<64,000,000 such that σ2(n) is a perfect square.
-}

{-
Note that σ2 is a multiplicative function.

for relatively prime m, n: s2(m*n) = s2(m) * s2(n)

s2(2^n) == 1 (mod 4)

For odd prime p,
s2(p) = p^2 + 1 == 2 (mod 4), not square
s2(2p) = s2(2) * s2(p) = 5 * (p^2 + 1) == p^2 + 1 == 2 (mod 4), not square
s2(3p) = s2(3) * s2(p) = 10 * (p^2 + 1) == 2 * 2 == 0 (mod 4), could be square
s2(4p) = s2(4) * s2(p) = 21 * (p^2 + 1) == 1 * 2 == 2 (mod 4), not square
s2(5p) = s2(5) * s2(p) = 26 * (p^2 + 1) == 2 * 2 == 0 (mod 4), could be square

s2(p^2) = 1 + p^2 + p^4 == 3 (mod 4)
s2(p^3) = 1 + p^2 + p^4 + p^6 == 4 (mod 4)

s2(2) = 5  = 5
s2(3) = 10 = 2*5
s2(4) = 21 = 3*7
s2(5) = 26 = 2*13
s2(7) = 50 = 2*5*5
s2(8) = 85 = 5*17
s2(9) = 91 = 7*13
s2(11) = 122 = 2*61
s2(13) = 170 = 2*5*17

s2(42) = 2500 = 50^2

quadratic residues mod n:
2: 0,1
3: 0,1,1
4: 0,1,0,1
5: 0,1,4,4,1
6: 0,1,4,3,4,1
7: 0,1,4,2,2,4,1
8: 0,1,4,1,0,1,4,1
9: 0,1,4,0,5,5,0,4,1
-}

z = 64000000

sigma2_odd_factors p =
  [ (e, fs) |
    e <- [1 ..],
    let x = sum [ (p^2)^k | k <- [0 .. e] ],
    let pf = prime_factorization x,
    let fs = [ q | (q, i) <- pf, odd i ]
  ]

prob211a nmax pmax =
  [ ((p, e), reverse fs) |
    p <- takeWhile (<pmax) primes,
    (e, fs) <- takeWhile (\(e, fs) -> p^e < nmax) (sigma2_odd_factors p)
  ]

prob211b nmax =
  sortOf (head . snd) (prob211a nmax nmax)

groupOf f = groupBy (\x y -> f x == f y)

prob211c nmax =
  concat $ filter long $ groupOf (head . snd) (prob211b nmax)

long (_ : _ : _) = True
long _ = False

-- prob211d nmax = map (\(a,b,c) -> a) (prob211c nmax)

---------------------------------------------------------

mult_pf [] ys = ys
mult_pf xs [] = xs
mult_pf xs@((p,i):xs') ys@((q,j):ys') =
  case compare p q of
    LT -> (p,i) : mult_pf xs' ys
    EQ -> (p,i+j) : mult_pf xs' ys'
    GT -> (q,j) : mult_pf xs ys'

sigma2_pfs ps =
  [ zip xs pfs |
    p <- ps,
    let xs = iterate (*p) 1,
    let ys = map (^2) xs,
    let zs = scanl1 (+) ys,
    let pfs = map prime_factorization zs
  ]

prob211 :: Int -> [Int]
prob211 nmax = f nmax (1, []) (sigma2_pfs ps0)
  where
    ps0 = reverse $ takeWhile (<= nmax`div`3) primes
    f z (n, pf) [] = if all even (map snd pf) then [n] else []
    f z (n, pf) (t:ts) = do
      (n', pf') <- takeWhile (\(n,_) -> n <= z) t
      f (z`div`n') (n*n', mult_pf pf pf') ts

---------------------------------------------------------

sym_diff [] ys = ys
sym_diff xs [] = xs
sym_diff xs@(x:xs') ys@(y:ys') =
  case compare x y of
    LT -> x : sym_diff xs' ys
    EQ -> sym_diff xs' ys'
    GT -> y : sym_diff xs ys'

sigma2_pfs' ps =
  [ zip xs pfs |
    p <- ps,
    let xs = iterate (*p) 1,
    let ys = map (^2) xs,
    let zs = scanl1 (+) ys,
    let pfs = [ [ q | (q,e) <- prime_factorization z, odd e ] | z <- zs ]
  ]

prob211' :: Int -> [Int]
prob211' nmax = f nmax (1, []) (sigma2_pfs' ps0)
  where
    ps0 = reverse $ takeWhile (<= nmax`div`3) primes
    f z (n, pf) [] = if null pf then [n] else []
    f z (n, pf) (t:ts) = do
      (n', pf') <- takeWhile (\(n,_) -> n <= z) t
      f (z`div`n') (n*n', sym_diff pf pf') ts

prob211'' :: Int -> [Int]
prob211'' nmax = g [(nmax, (1, []), (sigma2_pfs' ps0))]
  where
    ps0 = reverse $ takeWhile (<= nmax`div`3) primes
    g [] = []
    g ((z, (n, pf), []) : rest) =
      if null pf then n : g rest else g rest
    g ((z, (n, pf), (t:ts)) : rest) = g (add_new xs)
      where
        xs = takeWhile (\(n,_) -> n <= z) t
        add_new [] = rest
        add_new ((n', pf'):ys) =
          (z`div`n', (n*n', sym_diff pf pf'), ts) : add_new ys

{-
[1,42,246,287,728,1434,1673,1880,4264,6237,9799,9855,18330,21352,21385,24856,36531,39990,46655,57270,66815,92664,
125255,156570,182665,208182,212949,242879,273265,380511,391345,411558,539560,627215,693160,730145,741096,773224,814463,931722,992680
-}
        
---------------------------------------------------------------
-- evaluate a multiplicative function for all values up to nmax

-- multiplicative :: ((Int, Int) -> b) -> (b -> b -> b) -> [Int] -> [(Int, b)]
multiplicative f mult z ps0 = eval ps0
  where
    eval [] = []
    eval (p:ps) = takeWhile ((<= z) . fst) (foldr (unionOf fst) r1 rs)
      where
        es = zip (takeWhile (<= z) (iterate (*p) 1)) [(0::Int) ..]
        r1 = map (\(pe,e) -> (pe, f (p,e))) es
        rs = map (\(pe, x) -> [ (pe*n, mult x y) | (n, y) <- xs ]) r1
        xs = eval ps

-- prob211z :: Int -> [Int] -> [(Int, [(Int, Int)])]
prob211z :: Int -> [Int]
prob211z nmax = ys
  where
    ps0 = takeWhile (< nmax`div`3) primes
    f (p,e) = prime_factorization $ sum [ (p^2)^i | i <- [0 .. e] ]
    xs = multiplicative f mult_pf nmax ps0
    ys = [ n | (n, pf) <- xs, all even (map snd pf) ]

---------------------------------------------
sigma2_pf :: Integer -> [(Integer, Int)]
sigma2_pf n = f pf
  where
    f [] = []
    f (pe:pf') = mult_pf (g pe) (f pf')
    g (p,e) = prime_factorization $ sum [ (p^2)^i | i <- [0 .. e] ]
    pf = prime_factorization n

prob211_foo :: [Integer]
prob211_foo =
  [ n |
    n <- [1..],
    let pf = sigma2_pf n,
    all even (map snd pf) ]

prob211_bar :: [Integer] -> [Integer]
prob211_bar ns =
  [ n |
    n <- ns,
    let pf = prime_factorization n,
    let s = product (map g pf),
    is_square s ]
  where
    g (p,e) = sum [ (p^2)^i | i <- [0 .. e] ]

is_square x =
  x `mod` 4 <= 1 &&
  (x+1) `mod` 5 <= 2 &&
  (square_root x)^2 == x

 
{-
-}

{-
for odd prime p,
(1 + p^2) is always a multiple of 2.
(1 + p^2) is never a multiple of q, for prime q == 3 (mod 4).
(because -1 must be a quadratic residue mod q)

s2(2) = 5
s2(3) = 2 5
s2(5) = 2 13
s2(7) = 2 5 5
s2(8) = 5 17
s2(11) = 2 61
s2(13) = 2 5 17
s2(23) = 2 5 53
s2(27) = 2 2 5 41
s2(31) = 2 13 37
s2(41) = 2 29 29
s2(43) = 2 5 37
s2(47) = 2 5 13 17
s2(73) = 2 5 13 41
s2(81) = 11 61
s2(83) = 2 5 13 53
s2(157) = 2 5 5 17 29
s2(239) = 2 13 13 13 13
s2(307) = 2 5 5 5 13 29
56215914 was missed!

[1,42,246,287,728,1434,1673,1880,4264,6237,9799,9855,18330,21352,21385,24856,36531,39990,46655,57270,66815,92664,125255,156570,182665,208182,212949,242879,273265,380511,391345,411558,539560,627215,693160,730145,741096,773224,814463,931722,992680,1069895,1087009,1143477,1166399,1422577,1592935,1815073,2281255,2544697,2713880,2722005,2828385,3054232,3132935,3145240,3188809,3508456,4026280,4647985,4730879,5024488,5054015,5143945,5260710,5938515,6128024,6236705,6366767,6956927,6996904,7133672,7174440,7538934,7736646,7818776,8292583,8429967,8504595,8795423,9026087,9963071,11477130,11538505,11725560,12158135,12939480,12948776,13495720,13592118,13736408,15203889,15857471,16149848,16436490,16487415,16909849,18391401,18422120,20549528,20813976,20871649,21251412,22713455,23250645,23630711,24738935,26338473,26343030,26594568,28113048,29429144,29778762,29973414,30666090,30915027,34207446,34741889,34968983,35721896,37113593,37343065,38598255,39256230,42021720,44935590,45795688,45798935,48988758,49375521,51516049,51912289,52867081,56215914,59748234,61116363,62158134,63286535]
-}
-- Correct answer: 1922364685