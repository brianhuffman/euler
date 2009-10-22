module Euler257 where
import SquareRoot

{-

Problem 257
Angular Bisectors

26 September 2009

Given is an integer sided triangle ABC with sides a ≤ b ≤ c. (AB = c,
BC = a and AC = b).

The angular bisectors of the triangle intersect the sides at points E,
F and G (see picture below).

           C
           *_
          /| -_
         / \   -_
      G /   |    -_ F
       *_   \     _*_
      / \--__| _--   -_
     /   \  _*-__      -_
    /    _--  |  --__    -_
   /  _--   \ \      --__  -_
  /_--       \ |         --__-_
 *-------------*---------------*
A              E                B

The segments EF, EG and FG partition the triangle ABC into four
smaller triangles: AEG, BFE, CGF and EFG.

It can be proven that for each of these four triangles the ratio
area(ABC)/area(subtriangle) is rational.  However, there exist
triangles for which some or all of these ratios are integral.

How many triangles ABC with perimeter≤100,000,000 exist so that the
ratio area(ABC)/area(AEG) is integral?

-}

{-

area(ABC)/area(AEG) = AB/AE * AC/AG


From Angle Bisector Theorem:

BE/AE = BC/AC = a/b
CG/AG = BC/AB = a/c

AB/AE = (a+b)/b
AC/AG = (a+c)/c

area(ABC)/area(AEG) = (a+b)(a+c) / (bc)


For ratio to be integral, we must have

kbc = (a+b)(a+c)

for integers a <= b <= c < a+b, and 0 < k.

If b and c have a common prime factor p, then p must also divide a.
Thus, consider only cases where a, b, c are coprime.

------------------------------------------

kbc = (a+b)(a+c)
kbc <= (b+b)(c+c)  (Since a <= b <= c)
kbc <= 4bc
k <= 4

k = 1 is not possible.
k = 4 is only possible when a = b = c.

Other cases are k=2 and k=3.

==========================================

Case k = 2:

2bc = (a+b)(a+c)
2bc = a^2 + ab + ac + bc
bc = a^2 + ab + ac

Let s = b-a, and t = c-a.
So b = a+s, and c = a+t.

(a+s)(a+t) = a^2 + a(a+s) + a(a+t)
a^2 + as + at + st = a^2 + a^2 + as + a^2 + at
st = 2a^2.

c < a + b
a + t < 2a + s
s(a + t) < s(2a + s)
as + st < s(2a + s)
as + 2a^2 < s(2a + s)
a(2a + s) < s(2a + s)
a < s

If s and t have a common prime factor p, then p^2 divides 2a^2, and
thus p divides a.  So we only need to consider coprime s and t.

(s,t) or (t,s) = (2m^2, n^2) for coprime m, n, where n is odd.

(a,b,c) or (a,c,b) = (mn, mn+2m^2, mn+n^2)

We have a < s, and a < t.
Thus mn < 2m^2 and mn < n^2.
Finally, n < 2m and m < n.

(m,n) (s,t)    (a,b,c)
--------------------------
(2,3) (8,9)    (6,14,15)
(3,5) (18,25)  (15,33,40)
(4,5) (25,32)  (20,45,52)
(4,7)


==========================================

Case k = 3:

3bc = (a+b)(a+c)
3bc = a^2 + ab + ac + bc
2bc = a^2 + ab + ac

let s = 2b-a, so 2b = a+s, b = (a+s)/2
let t = 2c-a, so 2c = a+t, c = (a+t)/2

2bc = a^2 + ab + ac
(2b)(2c) = 2a^2 + a(2b) + a(2c)
(a+s)(a+t) = 2a^2 + a(a+s) + a(a+t)
a^2 + as + at + st = 2a^2 + a^2 + as + a^2 + at
st = 3a^2.

c < a + b
2c < 2a + 2b
a + t < 3a + s
s(a + t) < s(3a + s)
as + st < s(3a + s)
as + 3a^2 < s(3a + s)
a(3a + s) < s(3a + s)
a < s

If s and t have an odd common prime factor p, then p^2 divides 3a^2,
and thus p divides a.  Finally, this means that p divides b and c as
well.  So we only need to consider gcd(s,t) = {1,2}.

*** Case gcd(s,t) = 1 ***

(s,t) or (t,s) = (3m^2, n^2) for coprime m, n; 3 does not divide n.

(a,b,c) or (a,c,b) = (mn, m(3m+n)/2, (m+n)n/2)

We have a < s, and a < t.
Thus mn < 3m^2 and mn < n^2.
Finally, n < 3m and m < n.

The quantities a+s and a+t must be even.
a+s = mn + 3m^2 = m(3m + n)
a+t = mn + n^2 = (m + n)n

m even, n even --> m,n not coprime!
m even, n odd -> a+t is odd!
m odd, n even -> a+s is odd!
m odd, n odd -> This case works.

n == 1 or 5 (mod 6)

(m,n)  (a,b,c)
---------------------
(3,5)  (15,20,21)
(3,7)  (21,24,35)
(5,7)  (35,42,55)

*** Case gcd(s,t) = 2 ***

st = 3a^2

(s,t) or (t,s) = (6m^2, 2n^2) for coprime m, n; 3 does not divide n.

(a,b,c) or (a,c,b) = (2mn, m(3m+n), (m+n)n)

(m,n)  (a,b,c)
---------------------
(1,2)  (4,5,6)
(3,4)  (24,28,39)
(2,5)  (20,22,35)
(3,5)  (30,40,42)  duplicate of (15,20,21)
(4,5)  (40,


s = 2b-a = 6
t = 2c-a = 8
6*8 = 48 = 3*4^2


3m^2, n^2

let s = 2b-a, so 2b = a+s, b = (a+s)/2
let t = 2c-a, so 2c = a+t, c = (a+t)/2


-}

type Z = Integer

prim_triangles2 :: [(Z, Z, Z)]
prim_triangles2 =
  [ (a, b, c) |
    i <- [1 ..],
    let n = 2*i + 1,
    let t = n^2,
    m <- [i+1 .. n-1],
    gcd m n == 1,
    let s = 2*m^2,
    let a = m*n,
    let b = a + s,
    let c = a + t
  ]

count_triangles2_upto :: Z -> [((Z, Z, Z), Z)]
count_triangles2_upto pmax =
  [ ((a, b, c), count) |
    i <- [1 .. square_root (pmax `div` 12)],
    let n = 2*i + 1,
    let t = n^2,
    m <- [i+1 .. n-1],
    gcd m n == 1,
    let s = 2*m^2,
    let perim = (2*m + n)*(m + n),
    let count = pmax `div` perim,
    let a = m*n,
    let b = a + s,
    let c = a + t
  ]

{-
a+b+c = mn + (mn + 2mm) + (mn + nn)
      = 2mm + 3mn + nn
      = (2m + n)(m + n)
      > (n + n)(n/2 + n)
      = 3n^2

      3n^2 < pmax
      3(2i+1)^2 < pmax
      3(4i^2 + 2i + 1) < pmax
      12i^2 < pmax
-}


prim_triangles3 :: [(Z, Z, Z)]
prim_triangles3 =
  [ triple |
    i <- [1 ..],
    n <- [3*i+1, 3*i+2],
    m <- [i+1 .. n-1],
    gcd m n == 1,
    let triple
          | odd m && odd n = (m*n, m*(3*m+n)`div`2, n*(m+n)`div`2)
          | otherwise      = (2*m*n, m*(3*m+n), n*(m+n))
  ]

count_triangles3_upto :: Z -> [((Z, Z, Z), Z)]
count_triangles3_upto pmax =
  [ ((a,b,c), count) |
    i <- [0 .. square_root (pmax `div` 12)],
    n <- [3*i+1, 3*i+2],
    m <- [i+1 .. n-1],
    gcd m n == 1,
    let (a,b,c)
          | odd m && odd n = (m*n, m*(3*m+n)`div`2, n*(m+n)`div`2)
          | otherwise      = (2*m*n, m*(3*m+n), n*(m+n)),
    let perim = a + b + c,
    let count = pmax `div` perim,
    count > 0
  ]

{-
2*perim = 2mn + m(3m+n) + n(m+n)
        = 2mn + 3mm + mn + mn + nn
        = 3mm + 4mn + nn
        = (3m + n)(m + n)
        > (n + n)(n/3 + n)
        > 8/3 n^2
        > 8/3 (3i)^2
        = 24 i^2
-}


count_triangles4_upto :: Z -> [((Z, Z, Z), Z)]
count_triangles4_upto pmax = [((1, 1, 1), pmax`div`3)]

count_triangles_upto :: Z -> Z
count_triangles_upto pmax =
  sum (map snd (count_triangles2_upto pmax)) +
  sum (map snd (count_triangles3_upto pmax)) +
  sum (map snd (count_triangles4_upto pmax))

brute_force :: Z -> [((Z, Z, Z), Z)]
brute_force m =
  [ ((a, b, c), q) |
    a <- [1 .. m`div`3],
    b <- [a .. (m-a)`div`2],
    c <- [b .. min (a+b-1) (m-a-b)],
    let (q, r) = ((a+b)*(a+c)) `divMod` (b*c),
    r == 0
  ]

main :: IO String
main = return $ show $ count_triangles_upto (10^8)

answer :: String
answer = "139012411"

{-

Up to perimeter 1000:
  k=2: 85
  k=3: 192
  k=4: 333
  total: 610
-}
