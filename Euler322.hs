module Euler322 where
import EulerLib
import qualified SortedList as S

{-

Problem 322
Binomial coefficients divisible by 10
30 January 2011

Let T(m, n) be the number of the binomial coefficients iCn that are
divisible by 10 for n ≤ i < m (i, m and n are positive integers). You
are given that T(10^9, 10^7-10) = 989697000.

Find T(10^18, 10^12-10).

-}

-- super slow brute force
prob322a m n = length [ i | i <- [n..m-1], choose i n `mod` 10 == 0 ]

{-

iCn = row i, column n of Pascal's triangle.

iCn is nonzero mod prime p if, when written in base p, every digit of
n is less than or equal to the corresponding digit of i.

Pascal's triangle mod 2:
[1]
[1,1]
[1,-,1]
[1,1,1,1]
[1,-,-,-,1]
[1,1,-,-,1,1]
[1,-,1,-,1,-,1]
[1,1,1,1,1,1,1,1]
[1,-,-,-,-,-,-,-,1]
[1,1,-,-,-,-,-,-,1,1]
[1,-,1,-,-,-,-,-,1,-,1]
[1,1,1,1,-,-,-,-,1,1,1,1]
[1,-,-,-,1,-,-,-,1,-,-,-,1]
[1,1,-,-,1,1,-,-,1,1,-,-,1,1]
[1,-,1,-,1,-,1,-,1,-,1,-,1,-,1]
[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
[1,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,1]
[1,1,-,-,-,-,-,-,-,-,-,-,-,-,-,-,1,1]
[1,-,1,-,-,-,-,-,-,-,-,-,-,-,-,-,1,-,1]
[1,1,1,1,-,-,-,-,-,-,-,-,-,-,-,-,1,1,1,1]
[1,-,-,-,1,-,-,-,-,-,-,-,-,-,-,-,1,-,-,-,1]
[1,1,-,-,1,1,-,-,-,-,-,-,-,-,-,-,1,1,-,-,1,1]
[1,-,1,-,1,-,1,-,-,-,-,-,-,-,-,-,1,-,1,-,1,-,1]
[1,1,1,1,1,1,1,1,-,-,-,-,-,-,-,-,1,1,1,1,1,1,1,1]
[1,-,-,-,-,-,-,-,1,-,-,-,-,-,-,-,1,-,-,-,-,-,-,-,1]
[1,1,-,-,-,-,-,-,1,1,-,-,-,-,-,-,1,1,-,-,-,-,-,-,1,1]
[1,-,1,-,-,-,-,-,1,-,1,-,-,-,-,-,1,-,1,-,-,-,-,-,1,-,1]
[1,1,1,1,-,-,-,-,1,1,1,1,-,-,-,-,1,1,1,1,-,-,-,-,1,1,1,1]
[1,-,-,-,1,-,-,-,1,-,-,-,1,-,-,-,1,-,-,-,1,-,-,-,1,-,-,-,1]
[1,1,-,-,1,1,-,-,1,1,-,-,1,1,-,-,1,1,-,-,1,1,-,-,1,1,-,-,1,1]
[1,-,1,-,1,-,1,-,1,-,1,-,1,-,1,-,1,-,1,-,1,-,1,-,1,-,1,-,1,-,1]
[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
[1,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,1]

Row (2n+0) = Row n, with i -> [i,0]
Row (2n+1) = Row n, with i -> [i,i]

Number of nonzero entries in row n = 2^k,
where k = number of 1's in binary representation of n.

Column (0) = all ones
Column (2n+0) = Column n, with i -> [i,i]
Column (2n+1) = Column n, with i -> [0,i]

Column n is periodic with period 2^k, for n < 2^k

Pascal's triangle mod 5:
[1]
[1,1]
[1,2,1]
[1,3,3,1]
[1,4,1,4,1]
[1,-,-,-,-,1]
[1,1,-,-,-,1,1]
[1,2,1,-,-,1,2,1]
[1,3,3,1,-,1,3,3,1]
[1,4,1,4,1,1,4,1,4,1]
[1,-,-,-,-,2,-,-,-,-,1]
[1,1,-,-,-,2,2,-,-,-,1,1]
[1,2,1,-,-,2,4,2,-,-,1,2,1]
[1,3,3,1,-,2,1,1,2,-,1,3,3,1]
[1,4,1,4,1,2,3,2,3,2,1,4,1,4,1]
[1,-,-,-,-,3,-,-,-,-,3,-,-,-,-,1]
[1,1,-,-,-,3,3,-,-,-,3,3,-,-,-,1,1]
[1,2,1,-,-,3,1,3,-,-,3,1,3,-,-,1,2,1]
[1,3,3,1,-,3,4,4,3,-,3,4,4,3,-,1,3,3,1]
[1,4,1,4,1,3,2,3,2,3,3,2,3,2,3,1,4,1,4,1]
[1,-,-,-,-,4,-,-,-,-,1,-,-,-,-,4,-,-,-,-,1]
[1,1,-,-,-,4,4,-,-,-,1,1,-,-,-,4,4,-,-,-,1,1]
[1,2,1,-,-,4,3,4,-,-,1,2,1,-,-,4,3,4,-,-,1,2,1]
[1,3,3,1,-,4,2,2,4,-,1,3,3,1,-,4,2,2,4,-,1,3,3,1]
[1,4,1,4,1,4,1,4,1,4,1,4,1,4,1,4,1,4,1,4,1,4,1,4,1]
[1,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,1]

Row (5n+0) = Row n, with i -> i*[1,0,0,0,0]
Row (5n+1) = Row n, with i -> i*[1,1,0,0,0]
Row (5n+2) = Row n, with i -> i*[1,2,1,0,0]
Row (5n+3) = Row n, with i -> i*[1,3,3,1,0]
Row (5n+4) = Row n, with i -> i*[1,4,1,4,1]

Number of nonzero entries in row n = product [ d+1 | d <- base 5 n ]

Column (0) = all ones
Column (5n+0) = Column n, with i -> i*[1,1,1,1,1]
Column (5n+1) = Column n, with i -> i*[0,1,2,3,4]
Column (5n+2) = Column n, with i -> i*[0,0,1,3,1]
Column (5n+3) = Column n, with i -> i*[0,0,0,1,4]
Column (5n+4) = Column n, with i -> i*[0,0,0,0,1]

Column n is periodic with period 5^k, for n < 5^k

--------------------------------------------------------

10^7-10 in base 2: 10011000 10010110 01110110 - 2^12 / 2^24 (1/4096)
10^7-10 in base 5: 10024444430                - 3000 / 5^11 (1/16276)

10^12-10 in base 2: 11101000 11010100 10100101 00001111 11110110
10^12-10 in base 5: 112340444444444430
2^18 / 2^40 are nonzero mod 2 (1 in 4 million)
4800 / 5^18 are nonzero mod 5 (1 in 800 million)
-}

pascal_mod :: Int -> [[Int]]
pascal_mod n = iterate f [1]
  where
    f xs = zipWith add ([0] ++ xs) (xs ++ [0])
    add x y = let z = x+y in if z>=n then z-n else z

base b n = f [] n
  where
    f ds 0 = ds
    f ds n = let (q, r) = n `divMod` b in f (r:ds) q

row_nonzero_positions b 0 = [0]
row_nonzero_positions b n =
  [ b*x+y | x <- row_nonzero_positions b q, y <- [0..r] ]
  where (q, r) = n `divMod` b

-- first value is always n
col_nonzero_positions b 0 = [0..]
col_nonzero_positions b n =
  [ x+y | x <- map (*b) (col_nonzero_positions b q), y <- [r..b-1] ]
  where (q, r) = n `divMod` b

-- length (takeWhile (<m) (col_nonzero_positions b n))
col_nonzeros_upto b n m = fst (f n m)
  where
    f 0 m = (m, True)
    f n m = (k*(b-rn) + x, nz && rn <= rm)
      where
        (qn, rn) = n `divMod` b
        (qm, rm) = m `divMod` b
        (k, nz) = f qn qm
        x = if nz then max 0 (rm-rn) else 0

is_nonzero b 0 i = True
is_nonzero b n i = rn <= ri && is_nonzero b qn qi
  where
    (qn, rn) = n `divMod` b
    (qi, ri) = i `divMod` b

odd_choose i 0 = True
odd_choose i n = (even n || odd i) && odd_choose (i`div`2) (n`div`2)

prob322' m n = length ks
  where
    ks2 = col_nonzero_positions 2 n
    ks5 = col_nonzero_positions 5 n
    ks10 = S.union ks2 ks5
    ks = takeWhile (<m) (dropWhile (<n) ks10)

prob322b m n = (m-n) - toInteger (length ks)
  where
    ks2 = col_nonzero_positions 2 n
    ks5 = col_nonzero_positions 5 n
    ks10 = S.union ks2 ks5
    ks = takeWhile (<m) ks10

prob322c m n = (m-n) - x2 - x5 + x10
  where
    x2 = col_nonzeros_upto 2 n m
    x5 = col_nonzeros_upto 5 n m
    ks5 = takeWhile (<m) (col_nonzero_positions 5 n)
    x10 = toInteger (length (filter (\i -> odd_choose i n) ks5))

main :: IO String
main = return $ show $ prob322c (10^18) (10^12-10)

answer :: String
answer = "999998760323313995"
