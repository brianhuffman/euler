module Euler233 where
import SquareRoot
import qualified Data.Map as Map
import Primes

{-
Problem 233
20 February 2009

Let f(N) be the number of points with integer coordinates that are on
a circle passing through (0,0), (N,0), (0,N), and (N,N).

It can be shown that f(10000) = 36.

What is the sum of all positive integers N ≤ 10^(11) such that
f(N) = 420 ?

-}

-- slow brute force implementation
points :: Integer -> Int
points n = f 1 (-1) 4
  where
    f x y t
      | 2*x > n = t
      | otherwise =
        case compare (x*(x-n) + y*(y-n)) 0 of
          LT -> f x (y-1) t
          EQ -> f (x+1) (y-1) (t+8)
          GT -> f (x+1) y t

-- points_of_pf (prime_factorization n) = points n
points_of_pf :: [(Integer, Int)] -> Int
points_of_pf pf = 4 + 8 * f [ e | (p,e) <- pf, good p ]
  where
    f [] = 0
    f (e : es) = (2*e+1) * f es + e

{-

points_of_pf n = 420
f [e | (p,e) <- pf, good p] = 52

To have 420 points, we must have the form

p1^1 * p2^2 * p3^3
OR
p1^3 * p2^7
OR
p1^2 * p2^10

-}


good :: Integer -> Bool
good p = p `mod` 4 == 1

good_primes :: [Integer]
good_primes = filter good primes

ok_multiples :: [Integer]
ok_multiples =
  [ n | n <- [1 ..], all (not . good . fst) (prime_factorization n) ]

primitive_420s :: Integer -> [Integer]
primitive_420s m =
  [ p1 * p2^2 * p3^3 |
    p3 <- takeWhile (\p -> p^3 <= m) good_primes,
    let p2max = square_root (m`div`(p3^3)),
    p2 <- takeWhile (<= p2max) good_primes,
    p2 /= p3,
    let p1max = m`div`(p3^3)`div`(p2^2),
    p1 <- takeWhile (<= p1max) good_primes,
    p1 /= p2, p1 /= p3 ]

primitive_420s' :: Integer -> [Integer]
primitive_420s' m =
  [ p1^3 * p2^7 |
    p2 <- takeWhile (\p -> p^7 <= m) good_primes,
    p1 <- takeWhile (\p -> p^3 * p2^7 <= m) good_primes,
    p1 /= p2 ]

primitive_420s'' :: Integer -> [Integer]
primitive_420s'' m =
  [ p1^2 * p2^10 |
    p2 <- takeWhile (\p -> p^10 <= m) good_primes,
    p1 <- takeWhile (\p -> p^2 * p2^10 <= m) good_primes,
    p1 /= p2 ]

all_420s :: Integer -> [Integer]
all_420s m =
  [ n*k |
    n <- primitive_420s m ++ primitive_420s' m ++ primitive_420s'' m,
    let kmax = m `div` n,
    k <- takeWhile (<=kmax) ok_multiples ]


type Z = Int

-- x^2 + y^2 = 2*n^2
triple_of_ratio :: Z -> Z -> (Z, Z, Z)
triple_of_ratio p q = (x, y, n)
  where
    x = 2*p*q + (p + q) * (q - p)
    y = 2*p*q + (p + q) * (p - q)
    n = p^2 + q^2

-- x and y will both be even iff p and q are both odd.

-- primitive_triples_upto :: Z -> [(Z, Z, Z)]
primitive_triples_upto m =
  [ (p, q, triple_of_ratio p q) |
    -- let pmax = square_root (m`div`5),
    let pmax = square_root (m`div`2),
    p <- [1 .. pmax],
    let qmax = square_root (m - p^2),
    -- q <- [2*p .. qmax],
    q <- [p+1 .. qmax],
    gcd p q == 1 ]

all_triples_upto m =
  [ (p,q,k,(a*k, b*k, c*k)) |
    (p,q,(a,b,c)) <- primitive_triples_upto m,
    -- b < 0,
    k <- [1 .. m `div` c] ]

multiplicities m =
  Map.fromListWith (+)
    [ (n, 1) | (p,q,(x,y,n)) <- primitive_triples_upto m ]

multiplicities' m =
  Map.fromListWith (+) 
    [ (n, 1) | (p,q,k,(x,y,n)) <- all_triples_upto m, n `mod` 50 == 0 ]

{-
The circle through points [ (0,N)  (N,N)  (0,0)  (N,0) ]
is centered at (N/2, N/2), with radius = N / sqrt(2).

Formula for the circle:
x(x - N) + y(y - N) = 0

Intersect circle with line through the origin: y = mx
x(x - N) + mx(mx - N) = 0
x^2 - xN + m^2x^2 - mxN = 0
(m^2 + 1)x^2 - N(m + 1)x = 0
(m^2 + 1)x - N(m + 1) = 0  -OR-  x = 0
x = N(m + 1) / (m^2 + 1)
y = N(m^2 + m) / (m^2 + 1)

Assume that the line has rational slope: m = p/q
x = N(p/q + 1) / ((p/q)^2 + 1)
y = N((p/q)^2 + p/q) / ((p/q)^2 + 1)
(multiply top and bottom by q^2)
x = N q(p + q) / (p^2 + q^2)
y = N p(p + q) / (p^2 + q^2)

For coprime p q, is p(p + q) / (p^2 + q^2) in lowest terms?
If p and q are both odd, then a factor of 2 can be canceled.
(No other common factors are possible.)
Similarly for q(p + q) / (p^2 + q^2).

For coprime p q, x and y are integers iff
N is a multiple of (p^2 + q^2)  (for one of p, q even)
N is a multiple of (p^2 + q^2)/2  (for p, q both odd)

-}


{-


(N/2 + x/2, N/2 + y/2) is on the circle iff (x^2 + y^2) = 2*N^2


(N/2 + x, N/2 + y) is on the circle iff 2(x^2 + y^2) = N^2


N with large f(N):
f(10000) = 36

Solutions to f(N) = 420:
------------------------
 718250 = 2 5 5 5 13 13 17
 939250 = 2 5 5 5 13 17 17
1225250 = 2 5 5 5 13 13 29
1436500 = 2 2 5 5 5 13 13 17  (2 * 718250)
1563250 = 2 5 5 5 13 13 37
1732250 = 2 5 5 5 13 13 41
1867450 = 2 5 5 13 13 13 17
1878500 = 2 2 5 5 5 13 17 17
2095250 = 2 5 5 5 17 17 29
2154750 = 2 3 5 5 5 13 13 17  (3 * 718250)
2239250 = 2 5 5 5 13 13 53
2450500 = 2 2 5 5 5 13 13 29
2577250 = 2 5 5 5 13 13 61
2673250 =
2733250 =
2817750 =
2873000 =  (4 * 718250)
2962250 =
3084250,3126500,3185650,3193450,3464500,3574250,3675750,3734900,3757000,3760250,3829250,4064450,4098250,4190500,4267250
4309500 =  (6 * 718250)
4407250,4449250,4478500,4503850,4605250,4689750,4774250,4901000
5027750 =  (7 * 718250)
5154500,5196750,5274250,5346500,5463250,5466500,5602350,5635500
5746000 =  (8 * 718250)
5788250,5818250,5822050,5924500,6168500,6253000,6285750,6295250,6371300,6386900,6430250
6464250 =  (9 * 718250)
6574750,6633250,6700850,6717750,6929000,7008250,7123850,7144250,7148500,7297250,7309250,7351500,7469800,7514000,7520500,7647250,7658500,7731750,7779250,7875250,
7900750 =  (11 * 718250)
8019050,8019750,8128900,8154250,8164250,8196500,8199750,8323250,8381000,8453250,8534500,8576750
8619000 =  (12 * 718250)
8620250,8814500,8886750,8898500,8957000,9007700,9089050,9129250,9210500,9252750,9379500,9548500,9556950,9580350,9675250,9776650,9802000,9844250
9898250 = 2 5 5 5 17 17 137
9925250 = 2 5 5 5 29 37 37

These generally seem to be of the form 2 * p1 * p2^2 * p3^3,
  where p1 == p2 == p3 == 1 (mod 4).

Otherwise they are multiples of numbers of this form.

Factors of 718250 can be written as sums of squares in 104 ways.
52 even, 52 odd.

Of the 52 even,
  15 have 5^1
  15 have 5^2
  15 have 5^3
  7 have 5^0

A good prime (or power) can be written as sum of squares in 1 way.
A product of 2 good primes can be written as sum of squares in 2 ways.
A product of 3 good primes can be written as sum of squares in 4 ways.

([(2,1),(5,1)],71825)
([(2,1),(5,1),(13,1)],5525)
([(2,1),(5,1),(13,1)],5525)
([(2,1),(5,1),(13,1),(17,1)],325)
([(2,1),(5,1),(13,1),(17,1)],325)
([(2,1),(5,1),(13,1),(17,1)],325)
([(2,1),(5,1),(13,1),(17,1)],325)
([(2,1),(5,1),(13,2)],425)
([(2,1),(5,1),(13,2)],425)
([(2,1),(5,1),(13,2),(17,1)],25)
([(2,1),(5,1),(13,2),(17,1)],25)
([(2,1),(5,1),(13,2),(17,1)],25)
([(2,1),(5,1),(13,2),(17,1)],25)
([(2,1),(5,1),(17,1)],4225)
([(2,1),(5,1),(17,1)],4225)
([(2,1),(5,2)],14365)
([(2,1),(5,2),(13,1)],1105)
([(2,1),(5,2),(13,1)],1105)
([(2,1),(5,2),(13,1),(17,1)],65)
([(2,1),(5,2),(13,1),(17,1)],65)
([(2,1),(5,2),(13,1),(17,1)],65)
([(2,1),(5,2),(13,1),(17,1)],65)
([(2,1),(5,2),(13,2)],85)
([(2,1),(5,2),(13,2)],85)
([(2,1),(5,2),(13,2),(17,1)],5)
([(2,1),(5,2),(13,2),(17,1)],5)
([(2,1),(5,2),(13,2),(17,1)],5)
([(2,1),(5,2),(13,2),(17,1)],5)
([(2,1),(5,2),(17,1)],845)
([(2,1),(5,2),(17,1)],845)
([(2,1),(5,3)],2873)
([(2,1),(5,3),(13,1)],221)
([(2,1),(5,3),(13,1)],221)
([(2,1),(5,3),(13,1),(17,1)],13)
([(2,1),(5,3),(13,1),(17,1)],13)
([(2,1),(5,3),(13,1),(17,1)],13)
([(2,1),(5,3),(13,1),(17,1)],13)
([(2,1),(5,3),(13,2)],17)
([(2,1),(5,3),(13,2)],17)
([(2,1),(5,3),(13,2),(17,1)],1)
([(2,1),(5,3),(13,2),(17,1)],1)
([(2,1),(5,3),(13,2),(17,1)],1)
([(2,1),(5,3),(13,2),(17,1)],1)
([(2,1),(5,3),(17,1)],169)
([(2,1),(5,3),(17,1)],169)
([(2,1),(13,1)],27625)
([(2,1),(13,1),(17,1)],1625)
([(2,1),(13,1),(17,1)],1625)
([(2,1),(13,2)],2125)
([(2,1),(13,2),(17,1)],125)
([(2,1),(13,2),(17,1)],125)
([(2,1),(17,1)],21125)

([(5,1)],143650)
([(5,1),(13,1)],11050)
([(5,1),(13,1)],11050)
([(5,1),(13,1),(17,1)],650)
([(5,1),(13,1),(17,1)],650)
([(5,1),(13,1),(17,1)],650)
([(5,1),(13,1),(17,1)],650)
([(5,1),(13,2)],850)
([(5,1),(13,2)],850)
([(5,1),(13,2),(17,1)],50)
([(5,1),(13,2),(17,1)],50)
([(5,1),(13,2),(17,1)],50)
([(5,1),(13,2),(17,1)],50)
([(5,1),(17,1)],8450)
([(5,1),(17,1)],8450)
([(5,2)],28730)
([(5,2),(13,1)],2210)
([(5,2),(13,1)],2210)
([(5,2),(13,1),(17,1)],130)
([(5,2),(13,1),(17,1)],130)
([(5,2),(13,1),(17,1)],130)
([(5,2),(13,1),(17,1)],130)
([(5,2),(13,2)],170)
([(5,2),(13,2)],170)
([(5,2),(13,2),(17,1)],10)
([(5,2),(13,2),(17,1)],10)
([(5,2),(13,2),(17,1)],10)
([(5,2),(13,2),(17,1)],10)
([(5,2),(17,1)],1690)
([(5,2),(17,1)],1690)
([(5,3)],5746)
([(5,3),(13,1)],442)
([(5,3),(13,1)],442)
([(5,3),(13,1),(17,1)],26)
([(5,3),(13,1),(17,1)],26)
([(5,3),(13,1),(17,1)],26)
([(5,3),(13,1),(17,1)],26)
([(5,3),(13,2)],34)
([(5,3),(13,2)],34)
([(5,3),(13,2),(17,1)],2)
([(5,3),(13,2),(17,1)],2)
([(5,3),(13,2),(17,1)],2)
([(5,3),(13,2),(17,1)],2)
([(5,3),(17,1)],338)
([(5,3),(17,1)],338)
([(13,1)],55250)
([(13,1),(17,1)],3250)
([(13,1),(17,1)],3250)
([(13,2)],4250)
([(13,2),(17,1)],250)
([(13,2),(17,1)],250)
([(17,1)],42250)


Solutions to f(N) = 540:
------------------------
320450
961350
999050


-}

{-

1+x = 2(m + 1) / (m^2 + 1)
1+y = 2(m^2 + m) / (m^2 + 1)

m = p/q


1+x = 2((p/q) + 1) / ((p/q)^2 + 1)
1+y = 2((p/q)^2 + (p/q)) / ((p/q)^2 + 1)

1+x = 2(p*q + q^2) / (p^2 + q^2)
1+y = 2(p*q + p^2) / (p^2 + q^2)
--------------------------------
x^2 + y^2 = 2

x = 2*p*q - p^2 + q^2
y = 2*p*q + p^2 - q^2
x^2 + y^2 = 2


let N = p^2 + q^2
let A = N(1+x)
let B = N(1+y)

N = p^2 + q^2
A = 2(p*q + q^2)
B = 2(p^2 + p*q)

1+x = 2(p*q + q^2) / (p^2 + q^2)
1+y = 2(p^2 + p*q) / (p^2 + q^2)
--------------------------------
x^2 + y^2 = 2




if a^2 + b^2 = c^2, and coprime a b,

then (a,b,c) must be of the form
(q^2 - p^2, 2*p*q, q^2 + p^2)  OR
(2*p*q, q^2 - p^2, q^2 + p^2)
for p < q, coprime.

k | 2*p*q
k | q^2 - p^2
k | q^2 + p^2
-------------
k | 2*q^2
k | 2*p^2

The only possible common factor of (a,b,c) is 2;
this happens iff p+q is even.
In this case, the reduced pair is generated by a smaller p', q':
triple (2i) (2j) ~~ 4 * triple i j
triple (2i+1) (2j+1) ~~ 2 * triple (j-i) (j+i+1)

--- Derivation ---
2a^2 + 2b^2 = c^2
(2a/c)^2 + (2b/c)^2 = 2

Equivalent to finding rational solutions to x^2 + y^2 = 2
Solutions with 1 < x are 1-to-1 with rational slopes:
  m = slope of (-1,-1)-(x,y) =  (1+y) / (1+x),  0 < m < 1

m = (1+y) / (1+x)
m (1+x) = (1+y)
m (1+x) - 1 = y
(m (1+x) - 1)^2 = y^2
(m (1+x) - 1)^2 = 2 - x^2
m^2 (1+x)^2 - 2 m (1+x) + 1 = 2 - x^2
m^2 (x^2 + 2x + 1) - 2 m (1+x) + 1 = 2 - x^2
m^2 x^2 + 2 m^2 x + m^2 - 2 m - 2 m x + 1 = 2 - x^2
(m^2 + 1) x^2 + (2 m^2 - 2 m) x + (m^2 - 2 m - 1) = 0

x = (- m^2 + 2m + 1) / (m^2 + 1)  [assuming x /= -1]

m (1+x) - 1 = y

x = (- m^2 + 2m + 1) / (m^2 + 1)
1+x = (- m^2 + 2m + 1 + m^2 + 1) / (m^2 + 1)
1+x = 2(m + 1) / (m^2 + 1)
1+y = 2(m^2 + m) / (m^2 + 1)
y = (2m^2 + 2m - m^2 - 1) / (m^2 + 1)
y = (m^2 + 2m - 1) / (m^2 + 1)

1+x = 2(m + 1) / (m^2 + 1)
1+y = 2(m^2 + m) / (m^2 + 1)


A = m^2 + 1
B = 2(m^2 - m)
C = m^2 - 2m - 1

x = [ -B +- sqrt (B^2 - 4AC) ] / 2A
x = [ -2(m^2 - m) +- sqrt (4(m^2 - m)^2 - 4(m^2 + 1)(m^2 - 2m - 1)) ] / 2(m^2 + 1)
x = [ (m - m^2) +- sqrt ((m^2 - m)^2 - (m^2 + 1)(m^2 - 2m - 1)) ] / (m^2 + 1)
x = [ (m - m^2) +- sqrt (m^2(m - 1)^2 - (m^2 + 1)(m^2 - 2m - 1)) ] / (m^2 + 1)
x = [ (m - m^2) +- sqrt (m^2(m^2 - 2m + 1) - (m^2 + 1)(m^2 - 2m - 1)) ] / (m^2 + 1)
x = [ (m - m^2) +- sqrt (m^2(m^2 - 2m + 1) - m^2(m^2 - 2m - 1) - (m^2 - 2m - 1)) ] / (m^2 + 1)
x = [ (m - m^2) +- sqrt (m^4 - 2m^3 + m^2 - m^4 + 2m^3 + m^2 - m^2 + 2m + 1) ] / (m^2 + 1)
x = [ (m - m^2) +- sqrt (m^2 + 2m + 1) ] / (m^2 + 1)
x = [ (m - m^2) +- (m + 1) ] / (m^2 + 1)

x = (- m^2 + 2m + 1) / (m^2 + 1)
OR
x = - (m^2 + 1) / (m^2 + 1) = -1

x = [ (m - m^2) +- (m + 1) ] / (m^2 + 1)

m^2 (1+x)^2 = (1+y)^2
m^2 (1+x)^2 = y^2 + 2y + 1

m^2 (1+x)^2 = 1 - x^2
m^2 (1+x)^2 = (x+1)(1-x)    (assume x <> -1)
m^2 (1+x) = (1-x)
m^2 + m^2 x = 1 - x
x + m^2 x = 1 - m^2
x (1 + m^2) = 1 - m^2
x = (1 - m^2) / (1 + m^2)

m = y / (1+x)
m (1+x) = y
m^2 (1+x)^2 = y^2
m^2 (1+x)^2 = 1 - x^2
m^2 (1+x)^2 = (x+1)(1-x)    (assume x <> -1)
m^2 (1+x) = (1-x)
m^2 + m^2 x = 1 - x
x + m^2 x = 1 - m^2
x (1 + m^2) = 1 - m^2
x = (1 - m^2) / (1 + m^2)

y = m (1+x)
y = m (1 + (1 - m^2) / (1 + m^2))
y = m ((1 + m^2 + 1 - m^2) / (1 + m^2))
y = 2m / (1 + m^2)
-}

main :: IO String
main = return $ show $ sum $ all_420s (10^11)

answer :: String
answer = "271204031455541309"

-- 135780736563204140 WRONG!
-- 250535661911140496 WRONG!
-- 271176528227932684 WRONG!
-- 271204031455541309
