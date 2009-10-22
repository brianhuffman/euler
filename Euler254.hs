module Euler254 where
import Data.Char (digitToInt)
import Data.Array.Unboxed

{-
Problem 254
Sum of Digit Factorials

04 September 2009

Define f(n) as the sum of the factorials of the digits of n. For
example, f(342) = 3! + 4! + 2! = 32.

Define sf(n) as the sum of the digits of f(n). So sf(342) = 3 + 2 = 5.

Define g(i) to be the smallest positive integer n such that sf(n) =
i. Though sf(342) is 5, sf(25) is also 5, and it can be verified that
g(5) is 25.

Define sg(i) as the sum of the digits of g(i). So sg(5) = 2 + 5 = 7.

Further, it can be verified that g(20) is 267 and ∑ sg(i) for 1 ≤ i ≤
20 is 156.

What is ∑ sg(i) for 1 ≤ i ≤ 150?

-}

------------------------------------------------------------
-- factorial

fact_arr :: UArray Int Int
fact_arr = listArray (0,9) (scanl (*) 1 [1 .. 9])

fac :: Int -> Int
fac n = fact_arr ! n

------------------------------------------------------------
-- f and sf functions

type Digits = [(Int, Int)]
-- (digit, count)

f :: Digits -> Int
f ds = sum [ n * fac d | (d, n) <- ds ]

digSum :: Int -> Int
--digSum = sum . map digitToInt . show
digSum 0 = 0
digSum n = digSum q + r
  where (q, r) = n `divMod` 10

sf :: Digits -> Int
sf = digSum . f

sumDigits :: Digits -> Int
sumDigits ds = sum [ d * n | (d, n) <- ds ]


------------------------------------------------------------
{-

Finding smaller n with the same f(n).

* Permutations have the same result, so digits are always sorted.

* f(11) = f(2). Can be at most one 1.
* f(222) = f(3). Can be at most two 2s.
* f(3333) = f(4). Can be at most three 3s.
* f(44444) = f(5). Can be at most four 4s.
* f(555555) = f(6). Can be at most five 5s.
* f(6666666) = f(7). Can be at most six 6s.
* f(77777777) = f(8). Can be at most seven 7s.
* f(888888888) = f(9). Can be at most eight 8s.

Possible g values can be fully described by:
* How many 1s (0--1)
* How many 2s (0--2)
* How many 3s (0--3)
* How many 4s (0--4)
* How many 5s (0--5)
* How many 6s (0--6)
* How many 7s (0--7)
* How many 8s (0--8)
* How many 9s (unlimited)

The largest possible number of non-9 digits is 1 + 2 + ... + 8 = 36.

  0 -> 0
  1 -> 1
  2 -> 2
 12 -> 3
 22 -> 4
122 -> 5

fac' '3' = 6
fac' '4' = 6
fac' '5' = 3


-}

------------------------------------------------------------
-- g(i) is always a number with digits in ascending order.
-- sf preserves numbers modulo 9

ascending :: Int -> Int -> [Digits]
ascending l 9 = [[(9, l)]]
ascending l d =
  [ (d, n) : xs |
    n <- reverse [0 .. min l d],
    xs <- ascending (l-n) (d+1)
  ]

-- ascending_mod r l d =
--   [ xs <- ascending l d | sumDigits xs `mod` 9 == r ] 

ascending_mod :: Int -> Int -> Int -> [Digits]
ascending_mod 0 l 9 = [[(9, l)]]
ascending_mod r l d
  | d > 2 && r `mod` 3 /= 0 = []
  | d > 5 && r `mod` 9 /= 0 = []
  | otherwise =
      [ (d, n) : xs |
        n <- reverse [0 .. min l d],
        let r' = (r - n * fac d) `mod` 9,
        let l' = l - n,
        xs <- ascending_mod r' l' (d+1)
      ]

all_ascending_mod :: Int -> [Digits]
all_ascending_mod r =
  [ xs |
    l <- [1 ..],
    xs <- ascending_mod r l 1
  ]

type FS = (Int, Int)

ascending_mod_fs :: Int -> Int -> Int -> [FS]
ascending_mod_fs 0 l 9 = [(l * fac 9, l * 9)]
ascending_mod_fs r l d
  | d > 2 && r `mod` 3 /= 0 = []
  | d > 5 && r `mod` 9 /= 0 = []
  | otherwise =
      [ (f' + n * fac d, s' + n * d) |
        n <- reverse [0 .. min l d],
        let r' = (r - n * fac d) `mod` 9,
        let l' = l - n,
        (f',s') <- ascending_mod_fs r' l' (d+1)
      ]

all_ascending_mod_fs :: Int -> [FS]
all_ascending_mod_fs r =
  [ xs |
    l <- [1 ..],
    xs <- ascending_mod_fs r l 1
  ]

short_ascending_mod :: Int -> [Digits]
short_ascending_mod r =
  [ xs |
    l <- [1 .. 35],
    xs <- ascending_mod r l 1
  ]

------------------------------------------------------------
-- g and sg functions

short_fs :: [[(Int, Int)]]
short_fs =
  [ [ (f ds, sumDigits ds) | ds <- short_ascending_mod r ] |
    r <- [0 .. 8]
  ]

prefix_fs :: [[(Int, Int)]]
prefix_fs =
  [ [ (f ds, sumDigits ds) | ds <- ascending_mod r 36 1 ] |
    r <- [0 .. 8]
  ]

--sfs :: [[(Digits, Int)]]
--sfs = [ [ (ds, sf ds) | ds <- all_ascending_mod r ] | r <- [0..8] ]

g :: Int -> Digits
g i = head [ ds | ds <- all_ascending_mod r, sf ds == i ]
  where r = i `mod` 9

sg :: Int -> Int
--sg = sumDigits . g
sg i = head [ sn | (fn, sn) <- all_ascending_mod_fs r, digSum fn == i ]
  where r = i `mod` 9

sg' :: Int -> Int
sg' i = head $
  [ sumDigits ds | ds <- all_ascending_mod r, sf ds == i ] ++
  [ s' |
    n9 <- [0 ..],
    (f_ds, s_ds) <- prefix_fs !! r,
    let f' = f_ds + n9 * fac 9,
    let s' = s_ds + n9 * 9,
    digSum f' == i
  ]
  where r = i `mod` 9

------------------------------------------------------------

ascending_fs :: Int -> Int -> [FS]
ascending_fs l 9 = [(l * fac 9, l * 9)]
ascending_fs l d =
  [ (f' + n * fac d, s' + n * d) |
    let nmax = min l d,
    n <- [nmax, nmax-1 .. 0],
    (f',s') <- ascending_fs (l-n) (d+1)
  ]

-- sum_sg :: Int -> Int
sum_sg' imax =
    [ go b0 num fss |
      r <- [1 .. 9],
      let num = (imax + 9 - r) `div` 9,
      let fss = all_ascending_mod_fs (r `mod` 9)
    ]
  where
    b0 :: UArray Int Bool
    b0 = accumArray (||) False (1,imax) []
    go b 0 _ = []
    go b r ((facs, s) : fss)
      | i > imax  = go b r fss
      | b ! i     = go b r fss
      | otherwise = (i, s) : go (b // [(i, True)]) (r-1) fss
      where i = digSum facs

sum_sg imax = go b0 imax all_fss
  where
    b0 :: UArray Int Bool
    b0 = accumArray (||) False (1,imax) []
    all_fss =
      [ (i, s) |
        len <- [1..],
        (facs, s) <- ascending_fs len 1,
        let i = digSum facs,
        i < imax
      ]
    go b 0 _ = []
    go b r ((i, s) : fss)
      | b ! i     = go b r fss
      | otherwise = (i, s) : go (b // [(i, True)]) (r-1) fss


test = [ (i, s) | i <- [1 ..], let s = sg i ]

test' = [ (i, s) | i <- [1 ..], let s = sg' i ]

{-

digSum n == n (mod 9)


sf n
== f n
== sum . map fac . show $ n
== sum . map fac' . show $ n

fac '0' = 1
fac '1' = 1
fac '2' = 2
fac' '3' = 6
fac' '4' = 6
fac' '5' = 3
fac' '6' = 9
fac' '7' = 9
fac' '8' = 9
fac' '9' = 27


i       g(i)   sg(i)
====================
 1         1     1
 2         2     2
 3         5     5
 4        15     6
 5        25     7
 6         3     3
 7        13     4
 8        23     5
 9         6     6
10        16     7
11        26     8
12        44     8
13       144     9
14       256    13
15        36     9
16       136    10
17       236    11
18        67    13
19       167    14
20       267    15
21       349    16
22      1349    17
23      2349    18
24        49    13
25       149    14
26       249    15
27         9     9
28        19    10
29        29    11
30       129    12
31       229    13
32      1229    14
33        39    12
34       139    13
35       239    14
36      1239    15
37     13339    19
38     23599    28
39      4479    24
40     14479    25
41   2355679    37
42    344479    31
43   1344479    32
44   2378889    45
45  12378889    46
46  133378889   50
47  2356888899   66
48  12356888899   67
49  133356888899   71
50  12245677888899   84
51  34446666888899   89
52  134446666888899   90
53  12245578899999999   114
54  123345578899999999   118
55  1333666799999999999   134

-}

main :: IO String
main = return $ show $ sum $ map snd $ concat $ sum_sg' 70
-- main = return $ show $ sum [ sg i | i <- [1 .. 70] ]

answer :: String
answer = "???"
