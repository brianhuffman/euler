module Euler154 where
import EulerLib
import Data.Array.IArray
import Data.Array.Unboxed ( UArray )
import Data.Array ( Array )
import List
import Char
import Primes (divN)

{-
Problem 154
Pascal's pyramid.

12 May 2007

A triangular pyramid is constructed using spherical balls so that each
ball rests on exactly three balls of the next lower level.

Then, we calculate the number of paths leading from the apex to each
position: A path starts at the apex and progresses downwards to any
of the three spheres directly below the current position.

Consequently, the number of paths to reach a certain position is the
sum of the numbers immediately above it (depending on the position,
there are up to three numbers above it).

The result is Pascal's pyramid and the numbers at each level n are the
coefficients of the trinomial expansion (x + y + z)^n.

How many coefficients in the expansion of (x + y + z)^200000 are multiples
of 10^12? 
-}

-- See also Problem 148.

pascal_pyramid = iterate g [[1]]
  where
    f xs = zipWith (+) ([0] ++ xs) (xs ++ [0])
    g xss = zipWith (zipWith (+)) ([[0]] ++ map f xss) (xss ++ [repeat 0])

pascal_tri () = iter f [1]
  where
    iter f xs = seq (sum xs) (xs : iter f (f xs))
    f xs = zipWith p ([0] ++ xs) (xs ++ [0])
    p x y = (x + y) `mod` n
    n = 10^12

pascal200000 = pascal_tri () !! 200000

exponent_in_factorial p n =
  if n == 0 then 0 else
  let m = n `div` p in m + exponent_in_factorial p m

{-
for a+b+c = n,
coefficient of (x^a y^b z^c) in (x+y+z)^n is
P(a,b,c) = (a+b+c)! / (a! b! c!)

Pascal's triangle version:
P(a,b) = (a+b)! / (a! b!)

Pyramid function in terms of triangle function:
P(a,b,c) = P(a,b+c) * P(b,c)

Subproblem:
How many multiples of k are contained in row n of Pascal's triangle?


For prime p,
C(ap+b, cp+d) == C(a,c) * C(b,d)  (mod p)
P(ap+b, cp+d) == P(a,c) * P(b,d)  (mod p)



For a+b+c = 2,
P(a,b,c) == P(a*2^k, b*2^k, c*2^k)  (mod 4)

For a+b+c = 2^n,
P(a,b,c) == P(a*2^k, b*2^k, c*2^k)  (mod 2^(n+1)))




200000! has:
 2^199994
 5^49998

49998 = 40000 + 8000 + 1600 + 320 + 64 + 12 + 2

200000 in base 5: 22400000

2 + 22 + 224 + 2240 + 22400 + 224000 + 2240000
----------------------------------------------
2 + 22 + 220 + 2200 + 22000 + 220000 + 2200000
           4 +   40 +   400 +   4000 +   40000

200000 = 153125 - 46875
22400000 = 14400000 + 3000000

22400000
14400000

1 + 14 + 144 + 1440 + 14400 + 144000 + 1440000
     3 +  30 +  300 +  3000 +  30000 +  300000
----------------------------------------------
2 + 22 + 224 + 2240 + 22400 + 224000 + 2240000
-}

-- How many values in level m of the pyramid are multiples of 10^k?
{-
prob154a m k =
  [ (a,b,c) |
    a <- [0 .. m],
    let d2' = m2 - e2!a,
    let d5' = m5 - e5!a,
    b <- [0 .. m - a],
    let c = m - a - b,
    let d2 = d2' - e2!b - e2!c,
    let d5 = d5' - e5!b - e5!c,
    min d2 d5 >= k ]
  where
    e2, e5 :: UArray Int Int
    e2 = listArray (0, m) $ map (exponent_in_factorial 2) [0 .. m]
    e5 = listArray (0, m) $ map (exponent_in_factorial 5) [0 .. m]
    m2 = exponent_in_factorial 2 m
    m5 = exponent_in_factorial 5 m
-}

{-
-- How many values in level m of the pyramid are multiples of 10^k?
prob154 :: Int -> Int -> Int -- [((String, String, String), Int)]
prob154 m k = sum
  [ r |
    a <- [0 .. m `div` 3],
    let d2' = m2 - e2!a,
    let d5' = m5 - e5!a,
    b <- [a .. (m - a) `div` 2],
    let c = m - a - b,
    let d5 = d5' - e5!b - e5!c,
    d5 >= k,
    let d2 = d2' - e2!b - e2!c,
    d2 >= k,
    let r = if a == b || b == c then 3 else 6 ]
  where
    e2, e5 :: UArray Int Int
    e2 = listArray (0, m) $ map (exponent_in_factorial 2) [0 .. m]
    e5 = listArray (0, m) $ map (exponent_in_factorial 5) [0 .. m]
    m2 = exponent_in_factorial 2 m
    m5 = exponent_in_factorial 5 m
-}

-- How many values in level m of the pyramid are multiples of 10^k?
prob154 :: Int -> Int -> Int
prob154 m k = sum
  [ r |
    -- only consider a <= b <= c
    a <- [0 .. m `div` 3],
    let d2' = m2 - e2!a,
    let d5' = m5 - e5!a,
    max_carries2 5 (m-a) + (m5 - e5!a - e5!(m-a)) >= k,
    max_carries2 2 (m-a) + (m2 - e2!a - e2!(m-a)) >= k,
    b <- [a .. (m - a) `div` 2],
    let c = m - a - b,
    let d5 = d5' - e5!b - e5!c,
    d5 >= k,
    let d2 = d2' - e2!b - e2!c,
    d2 >= k,
    let r = if a == b || b == c then 3 else 6 ]
  where
    e2, e5 :: UArray Int Int
    e2 = listArray (0, m) $ map (exponent_in_factorial 2) [0 .. m]
    e5 = listArray (0, m) $ map (exponent_in_factorial 5) [0 .. m]
    m2 = exponent_in_factorial 2 m
    m5 = exponent_in_factorial 5 m

-- How many values in level m of the pyramid are multiples of 10^k?
prob154' :: Int -> Int -> Int
prob154' m k = sum
  [ r |
    -- only consider a >= b >= c
    a <- [(m + 2) `div` 3 .. m],
    let d2' = m2 - e2!a,
    let d5' = m5 - e5!a,
    max_carries2 5 (m-a) + (m5 - e5!a - e5!(m-a)) >= k,
    max_carries2 2 (m-a) + (m2 - e2!a - e2!(m-a)) >= k,
    b <- [(m - a + 1) `div` 2 .. min a (m-a)],
    let c = m - a - b,
--    if c < 0 then error (show (a,b,c)) else True,
    let d5 = d5' - e5!b - e5!c,
    d5 >= k,
    let d2 = d2' - e2!b - e2!c,
    d2 >= k,
    let r = if a == b || b == c then 3 else 6 ]
  where
    e2, e5 :: UArray Int Int
    e2 = listArray (0, m) $ map (exponent_in_factorial 2) [0 .. m]
    e5 = listArray (0, m) $ map (exponent_in_factorial 5) [0 .. m]
    m2 = exponent_in_factorial 2 m
    m5 = exponent_in_factorial 5 m

-- maximum number of carries for 2 numbers in base b, totalling n.
max_carries2 :: Int -> Int -> Int
max_carries2 b n = f n 0
  where
    f n c
      | q < c = q
      | c <= q = c' + f q c'
      where
        (q, r) = divMod n b
        c' = (2*(b-1) - r + c) `div` b

main :: IO String
main = return $ show $ prob154' 200000 12
-- 479742450

-- mapM_ print $ map last $ EulerLib.chunk 1000 $ scanl (\(a,b) (a',b') -> (a',b+b')) ((0,0,0),0) $ prob154 200000 12
-- 479742450


prob154b' m k =
  [ ((a,b,c),r) |
    a <- [0 .. m `div` 3],
    a `mod` 5 > 1,
    let d2' = m2 - e2!a,
    let d5' = m5 - e5!a,
    b <- [a .. min 78124 ((m - a) `div` 2)],
    (a `mod` 5) + (b `mod` 5) > 5,
    let c = m - a - b,
    let d2 = d2' - e2!b - e2!c,
    let d5 = d5' - e5!b - e5!c,
    d2 >= k, d5 >= k,
    let r = if a == b || b == c then 3 else 6 ]
  where
    e2 = funArray (0,m) (exponent_in_factorial 2)
    e5 = funArray (0,m) (exponent_in_factorial 5)
    m2 = exponent_in_factorial 2 m
    m5 = exponent_in_factorial 5 m

prob154c p n =
  [ (a,b,d) |
    a <- [0 .. n],
    let b = n - a,
    let ea = exponent_in_factorial p a,
    let eb = exponent_in_factorial p b,
    let d = en - ea - eb ]
  where
    en = exponent_in_factorial p n

prob154d p n =
  [ [ d |
      b <- [0 .. n - a],
      let c = n - a - b,
      let eb = exponent_in_factorial p b,
      let ec = exponent_in_factorial p c,
      let d = en - ea - eb - ec ]
    | a <- [0 .. n],
    let ea = exponent_in_factorial p a ]
  where
    en = exponent_in_factorial p n

--  15000 have 7
--  58000 have 6
-- 101600 have 5
--  20320 have 4
--   4064 have 3
--    808 have 2
--    164 have 1
--     45 have 0

base5 n = reverse (f n)
  where
    f 0 = []
    f n = case divMod n 5 of
      (q,r) -> intToDigit (fromIntegral r) : f q

--
max_carry b n c = min c' q
  where
    (q, r) = divMod n b
    c' = (3*(b-1) - r + c) `div` b

-- maximum number of carries for 3 numbers in base b, totalling n.
max_carries3 :: Int -> Int -> Int
max_carries3 b n = f n 0
  where
    f n c
      | q < c = q
      | c <= q = c' + f q c'
      where
        (q, r) = divMod n b
        c' = (3*(b-1) - r + c) `div` b

total3 :: Int -> Array Int [(Int, Int, Int)]
total3 b =
  accumArray (flip (:)) [] (0, 3*(b-1))
    [ (x+y+z, (x, y, z)) | x <- xs, y <- xs, z <- xs ]
  where xs = [0 .. b-1]
{-
-- (x,y,z) such that x+y+z = n has at least l total carries (base b).
carry_triples b l n = triples l n 0
  where
    digits = total3 b
    -- (x,y,z) such that x+y+z+c = n has at least l total carries (base b).
    triples l n c
      | l > max_carries b n c = []
--      | n <= c = []
      | n == c = if l <= 0 then [(0,0,0)] else []
      | otherwise =
        [ (b*x' + x, b*y' + y, b*z' + z) |
          c' <- [0 .. cmax],
          let t = b*c' + r - c,
          t >= 0,
          (x,y,z) <- digits ! t,
          (x',y',z') <- triples (l-c') n' c' ]
      where
        (n', r) = divMod n b
        cmax = min n' ((3*(b-1) - r + c) `div` b)

prob154_foo m k = length
  [ (a,b,c) |
    (a,b,c) <- carry_triples 5 k m,
    let d2 = m2 - e2!a - e2!b - e2!c,
    d2 >= k ]
  where
    e2, e5 :: UArray Int Int
    e2 = listArray (0, m) $ map (exponent_in_factorial 2) [0 .. m]
    e5 = listArray (0, m) $ map (exponent_in_factorial 5) [0 .. m]
    m2 = exponent_in_factorial 2 m
    m5 = exponent_in_factorial 5 m
-}

{-
The exponent of 5 in P(a,b,c) equals the total
number of carries in a+b+c, written in base 5.

 1122222
 --------
 00000322
 04444424
 12344144
 --------
 22400000

 1122222
 --------
 00010413
 03344434
 13434043
 --------
 22400000

To get a total of 12 for a+b+c = 22400000(base 5).

Three base-5 digits may have sums ranging from 0 to 12.

a1 a2 a3 a4 a5 a6 a7 a8
b1 b2 b3 b4 b5 b6 b7 b8
c1 c2 c3 c4 c5 c6 c7 c8
-----------------------
 1  6  7  8  8  8  8 10

a1 a2 a3 a4 a5 a6 a7 a8
b1 b2 b3 b4 b5 b6 b7 b8
c1 c2 c3 c4 c5 c6 c7 c8
-----------------------
 1  6  7  8  8  8  8 10

 2  2  2  2  2  2  2 
 0 10 12  8  8  8  8 10
-----------------------
 2  2  4  0  0  0  0  0

 1  1  2  2  2  2  2
 1  6  7  8  8  8  8 10
-----------------------
 2  2  4  0  0  0  0  0
-}

{-
Pyramid function in terms of triangle function:
P(a,b,c) = P(a,b+c) * P(b,c)

Subproblem:
How many multiples of k are contained in row n of Pascal's triangle?

From Wikipedia, "Binomial coefficient":
The prime divisors of n choose k can be interpreted as follows: if p is a
prime number and p^r is the highest power of p which divides n choose k, then
r is equal to the number of natural numbers j such that the fractional part of
k/p^j is bigger than the fractional part of n/p^j. In particular, n choose k
is always divisible by n/gcd(n,k).

pascal_primes p n k = snd (divN (choose n k) p)

-}

type RLE a = [(a, Int)]

listRLE :: RLE a -> [a]
listRLE ((x, n) : rs) = replicate n x ++ listRLE rs

sizeRLE :: RLE -> Int
sizeRLE = sum . map snd

scaleRLE :: Int -> RLE -> RLE
scaleRLE k rs = [ (x, k*n) | (x, n) <- rs ]

normRLE :: (Eq a) => RLE a -> RLE a
normRLE ((x, n) : (y, m) : rs)
  | x == y = normRLE ((x, n+m) : rs)
normRLE (xy : rs) = xy : normRLE rs
normRLE [] = []

mapRLE :: RLE -> RLE -> RLE
mapRLE rs zs = normRLE (concatMap f zs)
  where
    f (x, y) = concat (replicate x rs) ++ [(0, n*y)]
    n = sizeRLE rs

base :: Int -> Int -> [Int]
base b n = f n []
  where
    f 0 xs = xs
    f n xs = let (q,r) = divMod n b in f q (r:xs)

-- on row n of Pascal's triangle, what values are non-multiples of p?
pascal_nonmult_prime :: Int -> Int -> RLE
pascal_nonmult_prime p n
  | q == 0 = [(r+1, p-(r+1))]
  | otherwise = mapRLE rs qs
  where
    (q, r) = divMod n p
    qs = pascal_nonmult_prime p q
    rs = pascal_nonmult_prime p r

-- on row n of Pascal's triangle, what values are non-multiples of p^e?
pascal_nonmult_prime_power :: (Int, Int) -> Int -> RLE
pascal_nonmult_prime_power (p,e) n
  | q == 0 = [(r+1, p-(r+1))]
  | otherwise = mapRLE rs qs
  where
    (q, r) = divMod n p
    qs = pascal_nonmult_prime p q
    rs = pascal_nonmult_prime p r

triangle_mod n =
  map (map (\x -> (' ' : ['1'..]) !! fromInteger (x `mod` n)))
    pascal_triangle

triangle_power n = 
  map (map (\x -> ('-' : ['1'..]) !! snd (divN x n)))
    pascal_triangle

{-
Size A\a:
"-" | "-"

Size B\b:
"-11"  |  "-"
"--1"  |  "--"
"---"  |  "---"
[A, a+1, a+1]
[A,  A,  a+1]
[A,  A,   A ]

Size C\c:
"-22122122"  |  "-"
"--2112112"  |  "--"
"---111111"  |  "---"
"-11-22122"  |  "-11-"
"--1--2112"  |  "--1--"
"------111"  |  "------"
"-11-11-22"  |  "-11-11-"
"--1--1--2"  |  "--1--1--"
"---------"  |  "---------"
[B\b+1, B+1\b+1, B+1\b+1]
[ B\b,   B\b+1,  B+1\b+1]
[ B\b,    B\b,    B\b+1 ]

Size D:
"-33233233133233233133233233"  |  "-"
"--3223223113223223113223223"  |  "--"
"---222222111222222111222222"  |  "---"
"-11-33233122133233122133233"  |  "-11-"
"--1--3223112113223112113223"  |  "--1--"
"------222111111222111111222"  |  "------"
"-11-11-33122122133122122133"  |  "-11-11-"
"--1--1--3112112113112112113"  |  "--1--1--"
"---------111111111111111111"  |  "---------"
"-22122122-33233233133233233"  |  "-22122122-"
"--2112112--3223223113223223"  |  "--2112112--"
"---111111---222222111222222"  |  "---111111---"
"-11-22122-11-33233122133233"  |  "-11-22122-11-"
"--1--2112--1--3223112113223"  |  "--1--2112--1--"
"------111------222111111222"  |  "------111------"
"-11-11-22-11-11-33122122133"  |  "-11-11-22-11-11-"
"--1--1--2--1--1--3112112113"  |  "--1--1--2--1--1--"
"------------------111111111"  |  "------------------"
"-22122122-22122122-33233233"  |  "-22122122-22122122-"
"--2112112--2112112--3223223"  |  "--2112112--2112112--"
"---111111---111111---222222"  |  "---111111---111111---"
"-11-22122-11-22122-11-33233"  |  "-11-22122-11-22122-11-"
"--1--2112--1--2112--1--3223"  |  "--1--2112--1--2112--1--"
"------111------111------222"  |  "------111------111------"
"-11-11-22-11-11-22-11-11-33"  |  "-11-11-22-11-11-22-11-11-"
"--1--1--2--1--1--2--1--1--3"  |  "--1--1--2--1--1--2--1--1--"
"---------------------------"  |  "---------------------------"


   0 "-"
   1 "--"
   2 "---"
  10 "-11-"
  11 "--1--"
  12 "------"
  20 "-11-11-"
  21 "--1--1--"
  22 "---------"
 100 "-22122122-"
 101 "--2112112--"
 102 "---111111---"
 110 "-11-22122-11-"
 111 "--1--2112--1--"
 112 "------111------"
 120 "-11-11-22-11-11-"
 121 "--1--1--2--1--1--"
 122 "------------------"
 200 "-22122122-22122122-"
 201 "--2112112--2112112--"
 202 "---111111---111111---"
 210 "-11-22122-11-22122-11-"
 211 "--1--2112--1--2112--1--"
 212 "------111------111------"
 220 "-11-11-22-11-11-22-11-11-"
 221 "--1--1--2--1--1--2--1--1--"
 222 "---------------------------"
1000 "-33233233133233233133233233-"
1001 "--3223223113223223113223223--"
1002 "---222222111222222111222222---"
1010 "-11-33233122133233122133233-11-"
1011 "--1--3223112113223112113223--1--"
1012 "------222111111222111111222------"
1020 "-11-11-33122122133122122133-11-11-"
1021 "--1--1--3112112113112112113--1--1--"
1022 "---------111111111111111111---------"
1100 "-22122122-33233233133233233-22122122-"
1101 "--2112112--3223223113223223--2112112--"
1102 "---111111---222222111222222---111111---"
1110 "-11-22122-11-33233122133233-11-22122-11-"
1111 "--1--2112--1--3223112113223--1--2112--1--"
1112 "------111------222111111222------111------"
1120 "-11-11-22-11-11-33122122133-11-11-22-11-11-"
1121 "--1--1--2--1--1--3112112113--1--1--2--1--1--"
1122 "------------------111111111------------------"
1200 "-22122122-22122122-33233233-22122122-22122122-"
1201 "--2112112--2112112--3223223--2112112--2112112--"
1202 "---111111---111111---222222---111111---111111---"
1210 "-11-22122-11-22122-11-33233-11-22122-11-22122-11-"
1211 "--1--2112--1--2112--1--3223--1--2112--1--2112--1--"
1212 "------111------111------222------111------111------"
1220 "-11-11-22-11-11-22-11-11-33-11-11-22-11-11-22-11-11-"
1221 "--1--1--2--1--1--2--1--1--3--1--1--2--1--1--2--1--1--"
1222 "------------------------------------------------------"
2000 "-33233233133233233133233233-33233233133233233133233233-"
2001 "--3223223113223223113223223--3223223113223223113223223--"
2002 "---222222111222222111222222---222222111222222111222222---"
2010 "-11-33233122133233122133233-11-33233122133233122133233-11-"
2011 "--1--3223112113223112113223--1--3223112113223112113223--1--"
2012 "------222111111222111111222------222111111222111111222------"

-}