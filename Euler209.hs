module Euler209 where
import EulerLib
import Data.List (sort)

{-
A k-input binary truth table is a map from k input bits (binary digits,
0 [false] or 1 [true]) to 1 output bit. For example, the 2-input binary
truth tables for the logical AND and XOR functions are:

x 	y 	x AND y
0	0	0
0	1	0
1	0	0
1	1	1

x 	y 	x XOR y
0	0	0
0	1	1
1	0	1
1	1	0

How many 6-input binary truth tables, τ, satisfy the formula
τ(a, b, c, d, e, f) AND τ(b, c, d, e, f, a XOR (b AND c)) = 0

for all 6-bit inputs (a, b, c, d, e, f)? 
-}

{-
1-input binary truth tables: 2^(2^1) = 2^2 = 4
2-input binary truth tables: 2^(2^2) = 2^4 = 16
3-input binary truth tables: 2^(2^3) = 2^8 = 256
4-input binary truth tables: 2^(2^4) = 2^16 = 65536
5-input binary truth tables: 2^(2^5) = 2^32 = 4294967296
6-input binary truth tables: 2^(2^6) = 2^64 = 18446744073709551616

a  b  c  a XOR (b AND c)
0  0  0  0
0  0  1  0
0  1  0  0
0  1  1  1
1  0  0  1
1  0  1  1
1  1  0  1
1  1  1  0

τ(a, b, c, d, e, f) AND τ(b, c, d, e, f, a XOR (b AND c)) = 0

τ(0,0,0,0,0,0) AND τ(0,0,0,0,0,0)
τ(0,0,0,0,0,1) AND τ(0,0,0,0,1,0)
τ(0,0,0,0,1,0) AND τ(0,0,0,1,0,0)
τ(0,0,0,0,1,1) AND τ(0,0,0,1,1,0)
τ(0,0,0,1,0,0) AND τ(0,0,1,0,0,0)
τ(0,0,0,1,0,1) AND τ(0,0,1,0,1,0)
τ(0,0,0,1,1,0) AND τ(0,0,1,1,0,0)
τ(0,0,0,1,1,1) AND τ(0,0,1,1,1,0)
τ(0,0,1,0,0,0) AND τ(0,1,0,0,0,0)
τ(0,0,1,0,0,1) AND τ(0,1,0,0,1,0)
τ(0,0,1,0,1,0) AND τ(0,1,0,1,0,0)
τ(0,0,1,0,1,1) AND τ(0,1,0,1,1,0)
τ(0,0,1,1,0,0) AND τ(0,1,1,0,0,0)
τ(0,0,1,1,0,1) AND τ(0,1,1,0,1,0)
τ(0,0,1,1,1,0) AND τ(0,1,1,1,0,0)
τ(0,0,1,1,1,1) AND τ(0,1,1,1,1,0)
τ(0,1,0,0,0,0) AND τ(1,0,0,0,0,0)
τ(0,1,0,0,0,1) AND τ(1,0,0,0,1,0)
τ(0,1,0,0,1,0) AND τ(1,0,0,1,0,0)
τ(0,1,0,0,1,1) AND τ(1,0,0,1,1,0)
τ(0,1,0,1,0,0) AND τ(1,0,1,0,0,0)
τ(0,1,0,1,0,1) AND τ(1,0,1,0,1,0)
τ(0,1,0,1,1,0) AND τ(1,0,1,1,0,0)
τ(0,1,0,1,1,1) AND τ(1,0,1,1,1,0)
τ(0,1,1,0,0,0) AND τ(1,1,0,0,0,1)
τ(0,1,1,0,0,1) AND τ(1,1,0,0,1,1)
τ(0,1,1,0,1,0) AND τ(1,1,0,1,0,1)
τ(0,1,1,0,1,1) AND τ(1,1,0,1,1,1)
τ(0,1,1,1,0,0) AND τ(1,1,1,0,0,1)
τ(0,1,1,1,0,1) AND τ(1,1,1,0,1,1)
τ(0,1,1,1,1,0) AND τ(1,1,1,1,0,1)
τ(0,1,1,1,1,1) AND τ(1,1,1,1,1,1)
τ(1,0,0,0,0,0) AND τ(0,0,0,0,0,1)
τ(1,0,0,0,0,1) AND τ(0,0,0,0,1,1)
τ(1,0,0,0,1,0) AND τ(0,0,0,1,0,1)
τ(1,0,0,0,1,1) AND τ(0,0,0,1,1,1)
τ(1,0,0,1,0,0) AND τ(0,0,1,0,0,1)
τ(1,0,0,1,0,1) AND τ(0,0,1,0,1,1)
τ(1,0,0,1,1,0) AND τ(0,0,1,1,0,1)
τ(1,0,0,1,1,1) AND τ(0,0,1,1,1,1)
τ(1,0,1,0,0,0) AND τ(0,1,0,0,0,1)
τ(1,0,1,0,0,1) AND τ(0,1,0,0,1,1)
τ(1,0,1,0,1,0) AND τ(0,1,0,1,0,1)
τ(1,0,1,0,1,1) AND τ(0,1,0,1,1,1)
τ(1,0,1,1,0,0) AND τ(0,1,1,0,0,1)
τ(1,0,1,1,0,1) AND τ(0,1,1,0,1,1)
τ(1,0,1,1,1,0) AND τ(0,1,1,1,0,1)
τ(1,0,1,1,1,1) AND τ(0,1,1,1,1,1)
τ(1,1,0,0,0,0) AND τ(1,0,0,0,0,1)
τ(1,1,0,0,0,1) AND τ(1,0,0,0,1,1)
τ(1,1,0,0,1,0) AND τ(1,0,0,1,0,1)
τ(1,1,0,0,1,1) AND τ(1,0,0,1,1,1)
τ(1,1,0,1,0,0) AND τ(1,0,1,0,0,1)
τ(1,1,0,1,0,1) AND τ(1,0,1,0,1,1)
τ(1,1,0,1,1,0) AND τ(1,0,1,1,0,1)
τ(1,1,0,1,1,1) AND τ(1,0,1,1,1,1)
τ(1,1,1,0,0,0) AND τ(1,1,0,0,0,0)
τ(1,1,1,0,0,1) AND τ(1,1,0,0,1,0)
τ(1,1,1,0,1,0) AND τ(1,1,0,1,0,0)
τ(1,1,1,0,1,1) AND τ(1,1,0,1,1,0)
τ(1,1,1,1,0,0) AND τ(1,1,1,0,0,0)
τ(1,1,1,1,0,1) AND τ(1,1,1,0,1,0)
τ(1,1,1,1,1,0) AND τ(1,1,1,1,0,0)
τ(1,1,1,1,1,1) AND τ(1,1,1,1,1,0)
-}

b2i = foldl (\n b -> 2*n + (if b then 1 else 0)) 0

tuples =
  [ (x, y) |
    a <- [False, True],
    b <- [False, True],
    c <- [False, True],
    d <- [False, True],
    e <- [False, True],
    f <- [False, True],
    let g = a /= (b && c),
    let x = b2i [a,b,c,d,e,f],
    let y = b2i [b,c,d,e,f,g]
  ]

ends = sort $ concatMap (\(x,y) -> [x,y]) tuples

connect x = head $ map snd $ filter (\(a,b) -> a == x) tuples

{-
(1): [0]
(6): [1,2,4,8,16,32]
(46): [3,6,12,24,49,35,7,14,28,57,50,37,11,22,44,25,51,39,15,30,61,58,52,41,19,38,13,26,53,43,23,46,29,59,54,45,27,55,47,31,63,62,60,56,48,33]
(6): [5,10,20,40,17,34]
(3): [9,18,36]
(2): [21,42]

In a cycle of (n) elements, how many subsets are there that contain
no two adjacent elements?

n  f(n)  g(n)
------------
1   1     2
2   3     3
3   4     4
4   7 
5  

f(n) = g(n-3) + g(n-1)

In a sequence of (n) elements, how many subsets are there that contain
no two adjacent elements?

g(1) = 2
g(2) = 3
g(n) = g(n-1) + g(n-2)
-}

f 1 = 1
f 2 = 3
f 3 = 4
f n = g(n-3) + g(n-1)

g n = fibonacci (n+2)

subs6 =
  [ map bit [a,b,c,d,e,f] |
    a <- [False, True],
    b <- [False, True],
    not (a && b),
    c <- [False, True],
    not (b && c),
    d <- [False, True],
    not (c && d),
    e <- [False, True],
    not (d && e),
    f <- [False, True],
    not (e && f),
    not (a && f) ]
  where bit True = '1'
        bit False = '0'

subs10 =
  [ map bit [a,b,c,d,e,f,g,h,i,j] |
    a <- [False, True],
    b <- [False, True],
    not (a && b),
    c <- [False, True],
    not (b && c),
    d <- [False, True],
    not (c && d),
    e <- [False, True],
    not (d && e),
    f <- [False, True],
    not (e && f),
    g <- [False, True],
    not (f && g),
    h <- [False, True],
    not (g && h),
    i <- [False, True],
    not (h && i),
    j <- [False, True],
    not (i && j),
    not (j && a) ]
  where bit True = '1'
        bit False = '0'

prob209 = product $ map f [1,6,46,6,3,2]

main :: IO String
main = return $ show prob209
