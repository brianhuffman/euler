module Euler196 where
import EulerLib (triangle)
import SquareRoot
import PrimeArray
import Data.Array.Unboxed
import Data.Int (Int64)

{-
Problem 196
Prime triplets

30 May 2008

Build a triangle from all positive integers in the following way:

 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31 32 33 34 35 36
37 38 39 40 41 42 43 44 45
46 47 48 49 50 51 52 53 54 55
56 57 58 59 60 61 62 63 64 65 66
. . .

Each positive integer has up to eight neighbours in the triangle.

A set of three primes is called a prime triplet if one of the three
primes has the other two as neighbours in the triangle.

For example, in the second row, the prime numbers 2 and 3 are elements
of some prime triplet.

If row 8 is considered, it contains two primes which are elements of
some prime triplet, i.e. 29 and 31.

If row 9 is considered, it contains only one prime which is an element
of some prime triplet: 37.

Define S(n) as the sum of the primes in row n which are elements of any
prime triplet.

Then S(8)=60 and S(9)=37.

You are given that S(10000)=950007619.

Find  S(5678027) + S(7208785).
-}

{-
  1
{ 2} { 3}
  4  { 5}   6
{ 7}   8    9   10
{11}  12  {13}  14   15
 16  {17}  18  {19}  20   21
 22  {23}  24   25   26   27   28
{29}  30  {31}  32   33   34   35   36
{37}  38   39   40  {41}  42  (43)  44  45
 46  {47}  48  {49}  50   51   52  (53) 54  55
 56   57   58  {59}  60  (61)  62   63  64  65  66
. . .

Row(n) has length n.
Row(n) consists of [triangle(n-1) + 1 .. triangle(n)]

Row(10000) = [49995001 .. 50005000]
Row(5678027) = [16119992467352 .. 16119998145378]
Row(7208785) = [25983286983721 .. 25983294192505]

5678026 = 2 29 223 439
5678027 = 421 13487
5678028 = 2 2 3 3 109 1447

Neighbors of value k on row n:

(k-2n+1) (k-2n+2) (k-2n+3) (k-2n+4) (k-2n+5)
(k-n-1)  (k-n)    (k-n+1)  (k-n+2)  (k-n+3)
(k-2)    (k-1)    (k)      (k+1)    (k+2)
(k+n-2)  (k+n-1)  (k+n)    (k+n+1)  (k+n+2)
(k+2n-1) (k+2n)   (k+2n+1) (k+2n+2) (k+2n+3)

On an odd row, the neighbors of an odd entry are arranged thus:
0,1,0,1,0
1,0,1,0,1
1,0,1,0,1
0,1,0,1,0
0,1,0,1,0

The possible non-prime-triple arrangements are:
|  ? ?  |  - -  |  ? ?  |  ? ?  |
| ? - ? | ? * ? | ? - ? | ? - ? |
| ? * ? | ? * ? | - * ? | ? * - |
|  - -  |  - -  |  * -  |  - *  |
|  ? ?  |  ? ?  |  - ?  |  ? -  |

We must always test (k-n+1), (k+n-1), (k+n+1).
If only (k-n+1) is prime, we must also test (k-2n+2) and (k-2n+4).
If only (k+n-1) is prime, we must also test (k-2) and (k+2n).
If only (k+n+1) is prime, we must also test (k+2) and (k+2n+2).
-}

type Z = Int64

prime_table :: (Z, Z) -> UArray Z Bool
prime_table (i,j) = accumArray (const id) True (i,j) xs
  where
    ps = map fromIntegral (primes_upto (fromIntegral (square_root j)))
    xs = [ (n, False) |
           p <- ps,
           let k = max p ((i+p-1) `div` p),
           n <- [p*k, p*k+p .. j] ]

odd_prime_table :: (Z, Z) -> UArray Z Bool
odd_prime_table (i,j) = accumArray (const id) True (a,b) updates
  where
    ps = map fromIntegral (primes_upto (fromIntegral (square_root j)))
    (a, b) = (i`div`2, (j-1)`div`2)
    updates =
      [ (n, False) |
         -- p ranges over odd primes
         p <- tail ps,
         let p2 = p`div`2,
         -- n0 = least n>=a such that p divides 2n+1
         let n0 = (a+p-1) - (a+p2)`mod`p,
         let n1 = max n0 ((p^2)`div`2),
         n <- [n1, n1+p .. b] ]

n1, n2 :: Z
n1 = 5678027
n2 = 7208785

row :: Z -> [Z]
row n = [triangle(n-1) + 1 .. triangle n]

row_bnds :: Z -> (Z, Z)
row_bnds n = (triangle(n-1) + 1, triangle n)

-- only works for odd rows!
is_triplet_prime' prime n k
  | even n = error "even row"
  | otherwise = prime k && is_triplet
  where
    a = prime k
    b = prime (k-n+1)
    c = prime (k+n-1)
    d = prime (k+n+1)
    e = prime (k-2*n+2) || prime (k-2*n+4)
    f = prime (k-2) || prime (k+2*n)
    g = prime (k+2) || prime (k+2*n+2)
    is_triplet = case (b,c,d) of
          (False, False, False) -> False
          (True, False, False) -> e
          (False, True, False) -> f
          (False, False, True) -> g
          _ -> True

triplet_primes' n = filter (is_triplet_prime' prime n) (row n)
  where
    i = triangle (n-3) - 2
    j = triangle (n+2)
    arr = odd_prime_table (i,j)
    prime k = odd k && arr ! (k`div`2)

prob196 :: Z -> Z
prob196 n = sum (triplet_primes' n)

main :: IO String
main = return $ show $ prob196 n1 + prob196 n2

answer :: String
answer = "322303240771079935"

-- prob196 n1 = 79697256800321526
-- prob196 n2 = 242605983970758409
-- prob196 n1 + prob196 n2 = 322303240771079935

{-
Row(5678027) = [16119992467352 .. 16119998145378]
triplet primes on row 5678027:
16119992467633
16119992468357
16119992468603
16119992470801
16119992471537
...
4994 primes, sum 79697256800321526

Row(7208785) = [25983286983721 .. 25983294192505]
9337 primes, sum 242605983970758409
-}
