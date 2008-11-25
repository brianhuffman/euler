module Euler202 where
import Primes
import Word

{-
Problem 202
05 July 2008

Three mirrors are arranged in the shape of an equilateral triangle, with their
reflective surfaces pointing inwards. There is an infinitesimal gap at each
vertex of the triangle through which a laser beam may pass.

Label the vertices A, B and C. There are 2 ways in which a laser beam may enter
vertex C, bounce off 11 surfaces, then exit through the same vertex: one way
is shown below; the other is the reverse of that.

There are 80840 ways in which a laser beam may enter vertex C, bounce off
1000001 surfaces, then exit through the same vertex.

In how many ways can a laser beam enter at vertex C, bounce off 12017639147
surfaces, then exit through the same vertex?
-}

{-
lattice looks like this:

|\|\|\|
+-+-+-+
|\|\|\|
+-+-+-+
|\|\|\|
+-+-+-+
|\|\|\|
+-+-+-+

From origin (0,0) to point (x,y)
we cross (x-1) verticals
         (y-1) horizontals
         (x+y-1) diagonals

For a total of (2x+2y-3).

Exit is through same vertex iff
  (x-y) is a multiple of 3.

|\|\|\|\|\|\|\|\
+-+-o-+-+-o-+-+-
|\|\|\|\|\|\|\|\
+-o-+-+-o-+-+-o-
|\|\|\|\|\|\|\|\
o-+-+-o-+-+-o-+-
|\|\|\|\|\|\|\|\
+-+-o-+-+-o-+-+-
|\|\|\|\|\|\|\|\
+-o-+-+-o-+-+-o-
|\|\|\|\|\|\|\|\
o-+-+-o-+-+-o-+-

For example of 11 reflections,
n = 2x+2y-3 = 11
s = x+y = 7

1+6  (x-y = 5)
2+5  (x-y = 3) (hit!)
3+4  (x-y = 1)
4+3  (x-y = 1)
5+2  (x-y = 3) (hit!)
6+1  (x-y = 5)

For example of 1,000,001 reflections,
n = 2x+2y+3 = 1,000,001
s = x+y = 500,002

x and y must also be relatively prime
That is, x and y must be coprime to s.

Any common factor of x and y must also be a factor of 500,002

-------------------------------------------------------------------
x == y (mod 3)
2x == x+y (mod 3)
2x == s (mod 3)
4x == 2s (mod 3)
4x == 2s + 3 (mod 3)
4x == n (mod 3)
x == n (mod 3)
(similarly for y)

Reduced problem formulation:
Given n = 2s+3,
 how many values of 0 < x < s
 are coprime to s AND equiv to n (mod 3)?

-------------------------------------------------------------------

For final problem,
n = 2x+2y+3 = 12017639147
s = x+y = 6008819575
s = 5 5 11 17 23 29 41 47

s == 1 (mod 3)
n == 2 (mod 3)
x == 2 (mod 3)

x != 0 (mod 5)
x != 0 (mod 11)
x != 0 (mod 17)
x != 0 (mod 23)
x != 0 (mod 29)
x != 0 (mod 41)
x != 0 (mod 47)

all of these prime factors are == 2 (mod 3).

-}

n1 = 1000001
n2 = 12017639147

s n = (n + 3) `div` 2

s2 = 6008819575 -- = (n2+3)/2

prob202_estimate n = totient (s n) `div` 3

prob202_xs n =
  [ x |
    x <- [1 .. s`div`2],
    (x + s) `mod` 3 == 0,
    gcd x s == 1 ]
  where
    s = (n + 3) `div` 2

prob202_slow n = 2 * length (prob202_xs n)

-- requirement: n == 2 (mod 3)
prob202_xs' n =
  [ x |
    x <- [2, 5 .. m`div`2],
    -- x `mod` 3 == 2,
    gcd x m == 1 ]
  where
    m = (n + 3) `div` 2

{-
how many multiples of p up to k*p are equivalent to 2 (mod 3)?
p == 2 (mod 3)
2p, 5p, 8p .. are equiv to 2 (mod 3), for p==1 (mod 3)
1p, 4p, 7p .. are equiv to 2 (mod 3), for p==2 (mod 3)

2002939858 values == 2 (mod 3)

-}

ps = [2,5,11,17,23,29,41,47,53,59,71,83,89,101,107,113,131,137,149,167]
-- 6008819575 = 5 5 11 17 23 29 41 47

{-
answer is somewhere in the neighborhood of
1209002666

1209002624 -- RIGHT!
(twice 604501312)
-}

prob202 =
  [ (p, if t then n else -n) |
    d5 <- [0,1],
    d11 <- [0,1],
    d17 <- [0,1],
    d23 <- [0,1],
    d29 <- [0,1],
    d41 <- [0,1],
    d47 <- [0,1],
    let ds = [d5,d11,d17,d23,d29,d41,d47],
    let t = even (d5+d11+d17+d23+d29+d41+d47), -- p,k == 1 (mod 3)
    let p = product (zipWith (^) [5,11,17,23,29,41,47] ds),
    let k = s `div` p,
    let n = if t then k`div`3 else k`div`3 + 1]
  where s = 5*5*11*17*23*29*41*47

main :: IO String
main = return $ show $ sum $ map snd $ prob202

{-
wibble2 =
  [ (p, if t then n else -n) |
    d5 <- [0,1],
    d23 <- [0,1],
    d29 <- [0,1],
    d41 <- [0,1],
    d47 <- [0,1],
    let t = even (d5+d23+d29+d41+d47), -- p,k == 1 (mod 3)
    let p = product (zipWith (^) [5,23,29,41,47] [d5,d23,d29,d41,d47]),
    let k = m `div` p,
    let n = if t then k`div`3 else k`div`3 + 1]
  where
    m = 5*5*23*29*41*47
 
wibble' =
  [ (p, if t then n else -n) |
    d5 <- [0,1],
    d11 <- [0,1],
    d17 <- [0,1],
    d23 <- [0,1],
    d29 <- [0,1],
    let t = even (d5+d11+d17+d23+d29), -- p,k == 1 (mod 3)
    let p = product (zipWith (^) [5,11,17,23,29] [d5,d11,d17,d23,d29]),
    let k = m `div` p,
    let n = if t then k`div`3 else k`div`3 + 1]
  where
    m = 5*5*11*17*23*29

wibble4 =
  [ (p, if t then n else -n) |
    d5 <- [0,1],
    d11 <- [0,1],
    d17 <- [0,1],
    d23 <- [0,1],
    d29 <- [0,1],
    let t = even (d5+d11+d17+d23+d29), -- p,k == 1 (mod 3)
    let p = product (zipWith (^) [5,11,17,23,29] [d5,d11,d17,d23,d29]),
    let k = m `div` p,
    let n = if t then k`div`3 else k`div`3 + 1]
  where
    m = 5*5*11*11*11*17*23*29

wibble3 =
  [ (p, if t then n else -n) |
    d2 <- [0,1],
    d53 <- [0,1],
    d89 <- [0,1],
    let t = even (d2+d53+d89), -- p,k == 1 (mod 3)
    let p = product (zipWith (^) [2,53,89] [d2,d53,d89]),
    let k = m `div` p,
    let n = if t then k`div`3 else k`div`3 + 1]
  where
    m = 2*53*53*89

-}