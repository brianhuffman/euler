module Euler236 where
import Ratio

{-
Problem 236
Luxury Hampers

14 March 2009

Suppliers 'A' and 'B' provided the following numbers of products for
the luxury hamper market:

Product              'A'     'B'
Beluga Caviar        5248    640
Christmas Cake       1312    1888
Gammon Joint         2624    3776
Vintage Port         5760    3776
Champagne Truffles   3936    5664

Although the suppliers try very hard to ship their goods in perfect
condition, there is inevitably some spoilage – i.e. products gone bad.

The suppliers compare their performance using two types of statistic:

    * The five per-product spoilage rates for each supplier are equal
      to the number of products gone bad divided by the number of
      products supplied, for each of the five products in turn.

    * The overall spoilage rate for each supplier is equal to the
      total number of products gone bad divided by the total number of
      products provided by that supplier.

To their surprise, the suppliers found that each of the five
per-product spoilage rates was worse (higher) for 'B' than for 'A' by
the same factor (ratio of spoilage rates), m>1; and yet,
paradoxically, the overall spoilage rate was worse for 'A' than for
'B', also by a factor of m.

There are thirty-five m>1 for which this surprising result could have
occurred, the smallest of which is 1476/1475.

What's the largest possible value of m?  Give your answer as a
fraction reduced to its lowest terms, in the form u/v.

-}

prob236 =
  [ (m, [a1,b1,a4,b4,a6,b6]) |
    b1 <- [640, 639 .. 0],
    a1 <- [1 .. 5248],
    let m = (41*b1) % (5*a1),
    let u = numerator m,
    let v = denominator m,
    v < u,

    let m4 = m * (59%90),
    let u4 = numerator m4,
    let v4 = denominator m4,
    j <- [0 .. min (5760`div`v4) (3776`div`u4)],
    let a4 = j*v4,
    let b4 = j*u4,

    let m6 = m * (59%41),
    let u6 = numerator m6,
    let v6 = denominator m6,
    k <- [0 .. min (7872`div`v6) (11328`div`u6)],
    let a6 = k*v6,
    let b6 = k*u6,

    let a = a1+a4+a6,
    let b = b1+b4+b6,
    246 * a * v == 295 * b * u ]

main :: IO String
main = return $ showfrac mmax
  where
    mmax = maximum $ map fst prob236
    showfrac m = show (numerator m) ++ "/" ++ show (denominator m)

answer :: String
answer = "123/59"

-- 5 * a1 * u = 41 * b1 * v
--59 * a4 * u = 90 * b4 * v
--59 * a6 * u = 41 * b6 * v
--246 * (a1+a4+a6) * v = 295 * (b1+b4+b6) * u

{-
     'A'     'B'
1   5248     640
2   1312    1888
3   2624    3776
4   5760    3776
5   3936    5664
----------------
   18880   15744

'A' and 'B' have the same ratio for rows 2, 3, and 5.

Thus the problem is equivalent to:

     'A'     'B'
1   5248     640
4   5760    3776
6   7872   11328
----------------
   18880   15744

a1/5248 * u/v = b1/640
a4/5760 * u/v = b4/3776
a6/7872 * u/v = b6/11328
(a1+a4+a6)/18880 = (b1+b4+b6)/15744 * u/v

a1/b1 = 41/5  * v/u
a4/b4 = 90/59 * v/u
a6/b6 = 41/59 * v/u
(a1+a4+a6)/(b1+b4+b6) = 295/246 * u/v

m =  41/5  * b1/a1
m =  90/59 * b4/a4
m =  41/59 * b6/a6
m = 246/295 * (a1+a4+a6)/(b1+b4+b6)

 5 * a1 * u = 41 * b1 * v
59 * a4 * u = 90 * b4 * v
59 * a6 * u = 41 * b6 * v
246 * (a1+a4+a6) * v = 295 * (b1+b4+b6) * u

 5 * a1 * u = 41 * b1 * v
59 * a4 * u = 90 * b4 * v
59 * a6 * u = 41 * b6 * v
246 * (a1+a4+a6) * v = 295 * (b1+b4+b6) * u

 a1   a4   a6    b1    b4    b6
--------------------------------
 5u    0    0   -41v    0     0
  0   59u   0     0   -90v    0
  0    0   59u    0     0   -41v
246v 246v 246v -295u -295u -295u
-------------------------------------------------------
72570uv     0        0     -595074vv     0        0
   0     72570uv     0         0     -110700vv    0
   0        0     72570uv      0         0     -50430vv
72570uv  72570uv  72570uv  -87025uu  -87025uu  -87025uu
-------------------------------------------------------
72570uv     0        0     -595074vv     0        0
   0     72570uv     0         0     -110700vv    0
   0        0     72570uv      0         0     -50430vv
   0        0        0  (595074vv-87025uu) (110700vv-87025uu) (50430vv-87025uu)


246 = 6 * 41
295 = 5 * 59

----------------------------------------

1475 = 25 * 59
1476 = 36 * 41

u/v = 1476/1475
a1/b1 = 295/36
a4/b4 = 125/82
a6/b6 = 25/36
(a1+a4+a6)/(b1+b4+b6) = 6 / 5

     'A'     'B'
1   5248     640
4   5760    3776
6   7872   11328
----------------
   18880   15744

a1 = 295i
b1 = 36i
i < 18

a4 = 125j
b4 = 82j
j < 47

a6 = 25k
b6 = 36k
k < 315

(295i + 125j + 25k) * 5 = (36i + 82j + 36k) * 6
1475i + 625j + 125k = 216i + 492j + 216k
1259i + 133j = 91k




-}