module Euler263 where
import Primes
import Permutation
import Data.List
import qualified SortedList as S

{---------------------------------------------------------------------
Problem 263
An Engineer's Dream Come True

07 November 2009

Consider the number 6. The divisors of 6 are: 1,2,3 and 6.

Every number from 1 up to and including 6 can be written as a sum of
distinct divisors of 6:

1=1, 2=2, 3=1+2, 4=1+3, 5=2+3, 6=6.

A number n is called a practical number if every number from 1 up to
and including n can be expressed as a sum of distinct divisors of n.

A pair of consecutive prime numbers with a difference of six is called
a sexy pair (since "sex" is the Latin word for "six"). The first sexy
pair is (23, 29).

We may occasionally find a triple-pair, which means three consecutive
sexy prime pairs, such that the second member of each pair is the
first member of the next pair.

We shall call a number n such that :

    * (n-9, n-3), (n-3,n+3), (n+3, n+9) form a triple-pair, and
    * the numbers n-8, n-4, n, n+4 and n+8 are all practical, 

an engineers’ paradise.

Find the sum of the first four engineers’ paradises.

---------------------------------------------------------------------}

subset_sums :: [Int] -> [Int]
subset_sums [] = [0]
subset_sums (x : xs) = S.union ys (map (x+) ys)
  where ys = subset_sums xs

divisor_sums :: Int -> [Int]
divisor_sums n = subset_sums (list_divisors n)

practical :: Int -> Bool
practical n = [0 .. n] `isPrefixOf` divisor_sums n

practical' :: Int -> Bool
practical' n = sum_of_divisors n >= 2*n-1 && practical n

{---------------------------------------------------------------------
-- Properties of Practical Numbers

First practical numbers:

1 = 1 (primitive)
2 = 1 * 2^1
4 = 1 * 2^2
6 = 2 * 3^1
8 = 1 * 2^3
12 = 4 * 3^1
16 = 1 * 2^4
18 = 2 * 3^2
20 = 4 * 5^1
24 = 8 * 3^1
28 = 4 * 7^1
30 = 6 * 5^1
32 = 1 * 2^5
36 = 4 * 3^2
40 = 8 * 5^1
42 = 6 * 7^1
48 = 16 * 3^1
54 = 2 * 3^3
56 = 8 * 7^1
60 = 12 * 5^1
64 = 1 * 2^6
66 = 6 * 11^1
72 = 8 * 3^2
78 = 6 * 13^1
80 = 16 * 5^1
84 = 2 * 42
88 = 8 * 11^1
90 = 18 * 5^1
96 = 2 * 48
100 = 4 * 5^2
104 = 8 * 13^1
108 = 2 * 54
112 = 2 * 56
120 = 2 * 60
126 = 18 * 7^1
128 = 2 * 64
132 = 2 * 66
140 = 20 * 7^1
144 = 2 * 72
150 = 6 * 5^2
156 = 2 * 78
160 = 2 * 80
162 = 2 * 3^4
168 = 2 * 84

[1, 2, 4, 6, 8, 12, 16, 18, 20, 24, 28, 30, 32, 36, 40, 42, 48, 54,
 56, 60, 64, 66, 72, 78, 80, 84, 88, 90, 96, 100, 104, 108, 112, 120,
 126, 128, 132, 140, 144, 150, 156, 160, 162, 168, 176, 180, 192, 196,
 198, 200, 204, 208, 210, 216, 220, 224, 228, 234, ...

----------------------------------------------------------------------
First (even) impractical numbers:

10 = 2 5
14 = 2 7
22 = 2 11
26 = 2 13
34 = 2 17
44 = 4 11
46 = 2 23
50 = 2 5 5
52 = 4 13
58 = 2 29
62 = 2 31
68 = 4 17
70 = 2 5 7
74 = 2 37
76 = 4 19
82 = 2 41
86 = 2 43
92 = 4 23
94 = 2 47
98 = 2 7^2
102 = 6 17
106 = 2 53
110 = 2 5 11
114 = 

70 = 2 5 7
102 = 2 3 17
114 = 2 3 19
138 = 2 3 23
174 = 2 3 29
186 = 2 3 31
222 = 2 3 37

[10, 14, 22, 26, 34, 38, 44, 46, 50, 52, 58, 62, 68, 70, 74, 76, 82,
 86, 92, 94, 98, 102, 106, 110, 114, 116, 118, 122, 124, 130, 134,
 136, 138, 142, 146, 148, 152, 154, 158, 164, 166, 170, 172, 174, 178,
 182, 184, 186, 188, 190, 194, 202, 206, 212, 214, 218, 222, 226, 230,
 232, 236, 238, ...

practical n ==> sum_of_divisors n >= 2*n-1.



----------------------------------------------------------------------

Theorem: practical(a) ==> practical(b) ==> practical(a*b)

Proof.
Assume practical(a) and practical(b).
Then for all i<=a, and j<=b,
there exist distinct factors xs of a, ys of b,
such that sum xs = i, sum ys = j.

Let k be any number such that 0 <= k < a*b.
Then k can be written as i + j*a,
where 0 <= i < a and 0 <= j < b.
Obtain xs where distinct xs, all (divides a) xs, sum xs = i.
Obtain ys where distinct ys, all (divides b) ys, sum ys = j.
Let zs = xs ++ map (*a) ys.
Since all (< a) xs, and all (>= a) (map (*a) ys),
  we have distinct zs.
Also, all (divides (a*b)) zs.
Also, sum zs = sum xs + sum ys * a = i + j*a = k.
QED.

----------------------------------------------------------------------

Theorem:

Fix n and p, coprime.  Assume that all numbers i such that 0 <= i < p
can be written as the sum of distinct factors of n.  Then all numbers
0 <= k < p^2 can be written as the sum of distinct factors of n*p.

That is, R(n,p) --> R(n*p, p^2).

Proof:

Let k be any number such that 0 <= k < p^2.
Then k can be written as i + p*j, where 0 <= i < p, and 0 <= j < p.
Obtain xs where distinct xs, all (divides n) xs, sum xs = i.
Obtain ys where distinct ys, all (divides n) ys, sum ys = j.
Let zs = xs ++ map (*p) ys.
Since all (coprime p) xs, and none (coprime p) (map (*p) ys),
  we have distinct zs.
Also, all (divides (n*p)) zs.
Also, sum zs = sum xs + sum ys * p = i + j*p = k.
QED.

----------------------------------------------------------------------

Theorem:

Fix n and p, coprime.  Assume that all numbers i such that 0 <= i < p
can be written as the sum of distinct factors of n.  Then all numbers
0 <= k < p^(e+1) can be written as the sum of distinct factors of
n*p^e.

That is, R(n,p) --> R(n*p^e, p^(e+1))

Proof:

Let k be any number such that 0 <= k < p^(e+1).
Then k can be written as x[0] + p*x[1] + p^2*x[2] + ... p^e*x[e],
where 0 <= x[i] < p, for all 0 <= i <= e.
For each 0 <= i <= e, obtain xs[i],
where distinct xs[i], all (divides n) xs[i], sum xs[i] = x[i].

Let zs = xs[0] ++ map (* p^1) xs[1] ++ ... ++ map (*p^e) xs[e]
Each list is distinct, because p divides each list a different number
of times.  Also, all (divides (n*p)) zs.
Also, sum zs = x[0] + p*x[1] + p^2*x[2] + ... p^e*x[e] = k.
QED.

----------------------------------------------------------------------

Define R(n,a) <--> Every number in [1..a] can be written as a sum of
distinct divisors of n.

----------------------------------------------------------------------

Theorem:
  R(n,a) --> R(n,b) --> R(n*a, a*b)

Proof:

Let k be any number such that 0 <= k <= a*b.
Then k can be written as x + y*a,
where 0 <= x < a and 0 <= y <= b.
From R(n,a) obtain xs where distinct xs, all (divides n) xs, sum xs = x.
From R(n,b) obtain ys where distinct ys, all (divides n) ys, sum ys = y.
Let zs = xs ++ map (*a) ys.
Since all (< a) xs, and all (>= a) (map (*a) ys),
  we have distinct zs.
Also, all (divides (n*a)) zs.
Also, sum zs = sum xs + sum ys * a = x + y*a = k.
QED.

Corollary:
  R(n, max a b) --> R(n*a, a*b)

----------------------------------------------------------------------

Define S(n,x) <--> x can be written as a sum of distinct divisors of
n.

Theorem:
  Assume prime p does not divide n.
  Then S(n,x) --> S(n,y) --> S(p*n, p*x+y)

Obtain distinct xs where all (divides n) xs, sum xs = x.
Obtain distinct ys where all (divides n) ys, sum ys = y.
Let zs = map (p*) xs ++ ys.
Since all (coprime p) ys && none (coprime p) (map (*p) xs),
  we have distinct zs.
Also, all (divides (p*n)) zs.
Also, sum zs = p * sum xs + sum ys = p*x + y
QED.

Corollary:
  ~S(p*n, p*x+y) --> ~S(n,x) \/ ~S(n,y)

----------------------------------------------------------------------

An odd number > 1 cannot be practical.  There is no way for factors to
sum to 2.

Thus a == 1 (mod 2) is not practical.

A number with one 2, no 3s, and at least one larger factor in its
prime factorization cannot be practical.  Factors: 1, 2, (next factor
is at least 5).  There is no way for factors to sum to 4.

Thus a == 2 or 10 (mod 12) is not practical.

A number with two 2s, but no 3, 5 or 7 in its prime factorization
cannot be practical.  1, 2, 1+2=3, 4, 1+4=5, 2+4=6, 1+2+4=7, (next
factor is at least 11).  There is no way for factors to sum to 8.

Thus one of the following must hold: 8|a or 3|a or 5|a or 7|a.

A number with three 2s, but no 3, 5, 7, 11 or 13 in its prime
factorization cannot be practical.  There is no way for factors to sum
to 16.

Thus one of the following must be a factor: 16, 3, 5, 7, 11, or 13.

---------------------------------------------------------------------}

-- TODO: faster algorithm for testing practical numbers

easily_practical n

{-
R(3036,3036)

3036 = 66 * 23

R(66, max 23 66)
R(66, 66)

66 = 6 * 11

R(6, max 6 11)
R(6, 11)
R(6, 12)

6 = 2 * 3

R(2, max 3 4)

R(


R(44540, 44540)

R(340*131
-}

product_pf pf = product [ p^e | (p,e) <- pf ]

big_divisor n = product_pf (init (prime_factorization n))
   
-- By experiment, we have practical n --> practical (big_divisor n).


{--------------------------------------------------------------------

* the numbers n-8, n-4, n, n+4 and n+8 are all practical, 

None of these can be congruent to 2 or 10 (mod 12).

This rules out n == 2,6,10 (mod 12), i.e. n == 2 (mod 4) is ruled out.

Therefore, we must have n == 0 (mod 4).

* the numbers n-9, n-3, n+3, n+9 are all prime.

Primality requires:
* n == 1,2 (mod 3).
* n == 0 (mod 5).
* n == 0,1,6 (mod 7)

For these moduli, we have these possible values for
n-8, n-4, n, n+4 and n+8:

(mod 8)
  0  4  0  4  0
  4  0  4  0  4

(mod 3)
  2  0  1  2  0
  0  1  2  0  1

(mod 5)
  2  1  0  4  3

(mod 7)
  5  1  6  2  0
  6  4  0  5  1
  0  5  1  6  2

Each position must have a zero in one of these moduli.

Possible combination 1:
  4  0  4  0  4  (mod 8)
  2  0  1  2  0  (mod 3)
  2  1  0  4  3  (mod 5)
  0  5  1  6  2  (mod 7)
  n == 820 (mod 840)

Possible combination 2:
  4  0  4  0  4  (mod 8)
  0  1  2  0  1  (mod 3)
  2  1  0  4  3  (mod 5)
  5  1  6  2  0  (mod 7)
  n == 20 (mod 840)

---------------------------------------------------------------------}

candidates :: [Int]
candidates =
  [ n |
    k <- [0, 840 ..],
    n <- [k+20, k+820],
    is_prime (n-9),
    is_prime (n-3),
    is_prime (n+3),
    is_prime (n+9)
  ]
 
pre_paradises :: [Int]
pre_paradises =
  [ n |
    n <- candidates,
    practical n
  ]

paradises :: [Int]
paradises =
  [ n |
    n <- pre_paradises,
    practical (n-8),
    practical (n-4),
    practical (n+4),
    practical (n+8)
  ]

isn't :: (a -> Bool) -> a -> Bool
isn't p x = not (p x)
