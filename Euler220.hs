module Euler220 where

{-
Problem 220
06 December 2008

Let D_(0) be the two-letter string "Fa". For n≥1, derive D_(n) from
D_(n-1) by the string-rewriting rules:

"a" → "aRbFR"
"b" → "LFaLb"

Thus, D_(0) = "Fa", D_(1) = "FaRbFR", D_(2) = "FaRbFRRLFaLbFR", and so on.

These strings can be interpreted as instructions to a computer
graphics program, with "F" meaning "draw forward one unit", "L"
meaning "turn left 90 degrees", "R" meaning "turn right 90 degrees",
and "a" and "b" being ignored. The initial position of the computer
cursor is (0,0), pointing up towards (0,1).

Then D_(n) is an exotic drawing known as the Heighway Dragon of order
n. For example, D_(10) is shown below; counting each "F" as one step,
the highlighted spot at (18,16) is the position reached after 500
steps.

What is the position of the cursor after 10^(12) steps in D_(50) ?
Give your answer in the form x,y with no spaces.
-}


{-
a(1) = RFR
b(1) = LFL
a -> aRbFR
b -> LFaLb

LFa -> LFaRbFR
bFR -> LFaLbFR

Let c = LFa, d = bFR
c(1) = LFRFR  (net turn of R)
d(1) = LFLFR  (net turn of L)
c -> cRd
d -> cLd

D(n) contains 2^n moves.
D(n) is a prefix of D(n+1).

  1-2          2
  | |         / \
  0 3-4      0   4     0---4    0       0
      |         /          |     \      |
    6-5        6           |      \     |
    |           \          |       \    |
  C-7-8      C   8     C---8        8   |
  | | |     / \ /      |           /    |
E-D A-9    E   A       |          /     |
|           \          |         /      |
F-G          G         G        G       G

If we have
  dragon(n) = (x,y)
  dragon(n+1) = (u,v)

then we can calculate
  dragon(2n) = (x+y, y-x)
  dragon(2n+1) = (x+y, v-u)
  dragon(2n+2) = (u+v, v-u)

Alternate system in terms of absolute directions:
  n -> ne
  e -> se
  s -> sw
  w -> nw
-}

-- dragon' n = (dragon n, dragon (n+1))
dragon' 0 = ((0, 0), (0, 1))
dragon' n
  | even n = ((x+y, y-x), (x+y, v-u))
  | otherwise = ((x+y, v-u), (u+v, v-u))
  where
    ((x, y), (u, v)) = dragon' (n `div` 2)

dragon :: Integer -> (Integer, Integer)
dragon n = fst (dragon16 n)

main :: IO String
main = return (show x ++ "," ++ show y)
  where (x, y) = dragon (10^12)


{-
Representing ((x,y),(u,v)) by ((x,y),delta).

    N
    |
 W--+--E
    |
    S

n -> 2n, 2n+1
((x,y), N) -> ((x+y, y-x), N), ((x+y, y-x + 1), E)
((x,y), E) -> ((x+y, y-x), S), ((x+y, y-x - 1), E)
((x,y), S) -> ((x+y, y-x), S), ((x+y, y-x - 1), W)
((x,y), W) -> ((x+y, y-x), N), ((x+y, y-x + 1), W)

(x,y) -> (x+y, y-x) -> (2y, -2x) -> (2y-2x, -2x-2y) -> (-4x, -4y)

n -> ne -> nese -> neseswse -> neseswseswnwswse
e -> se -> swse -> swnwswse -> swnwnenwswnwswse
s -> sw -> swnw -> swnwnenw -> swnwnenwnesenenw
w -> nw -> nenw -> nesenenw -> neseswsenesenenw
-}

data Delta = N | E | S | W
  deriving (Eq, Show)

next_x :: Delta -> Integer -> Integer
next_x d x = case d of { N -> x; E -> x+1; S -> x; W -> x-1 }

next_y :: Delta -> Integer -> Integer
next_y d y = case d of { N -> y+1; E -> y; S -> y-1; W -> y }

d2n :: Delta -> Delta
d2n d = case d of { N -> N; E -> S; S -> S; W -> N }

d2n1 :: Delta -> Delta
d2n1 d = case d of { N -> E; E -> E; S -> W; W -> W }

dragon'' 0 = ((0, 0), N)
dragon'' n
  | even n = ((x+y, y-x), d2n d)
  | otherwise = ((x+y, next_y (d2n d) (y-x)), d2n1 d)
  where
    ((x, y), d) = dragon'' (n `div` 2)

-- Reducing mod 4
dragon4 0 = ((0, 0), N)
dragon4 n = x' `seq` y' `seq`
  case d of
    N -> case r of
           0 -> ((x'+0, y'+0), N)
           1 -> ((x'+0, y'+1), E)
           2 -> ((x'+1, y'+1), S)
           3 -> ((x'+1, y'+0), E)
    E -> case r of
           0 -> ((x'+0, y'+0), S)
           1 -> ((x'+0, y'-1), W)
           2 -> ((x'-1, y'-1), S)
           3 -> ((x'-1, y'-2), E)
    S -> case r of
           0 -> ((x'+0, y'+0), S)
           1 -> ((x'+0, y'-1), W)
           2 -> ((x'-1, y'-1), N)
           3 -> ((x'-1, y'+0), W)
    W -> case r of
           0 -> ((x'+0, y'+0), N)
           1 -> ((x'+0, y'+1), E)
           2 -> ((x'+1, y'+1), N)
           3 -> ((x'+1, y'+2), W)
  where
    (n', r) = divMod n 4
    ((x, y), d) = dragon4 n'
    (x', y') = (2*y, -2*x)

-- Reducing mod 16
dragon16 0 = ((0, 0), N)
dragon16 n = x' `seq` y' `seq`
  case d of
    N -> case r of
           0  -> ((x'+0, y'+0), N)
           1  -> ((x'+0, y'+1), E)
           2  -> ((x'+1, y'+1), S)
           3  -> ((x'+1, y'+0), E)
           4  -> ((x'+2, y'+0), S)
           5  -> ((x'+2, y'-1), W)
           6  -> ((x'+1, y'-1), S)
           7  -> ((x'+1, y'-2), E)
           8  -> ((x'+2, y'-2), S)
           9  -> ((x'+2, y'-3), W)
           10 -> ((x'+1, y'-3), N)
           11 -> ((x'+1, y'-2), W)
           12 -> ((x'+0, y'-2), S)
           13 -> ((x'+0, y'-3), W)
           14 -> ((x'-1, y'-3), S)
           15 -> ((x'-1, y'-4), E)
    E -> case r of
           0  -> ((x'+0, y'+0), S)
           1  -> ((x'+0, y'-1), W)
           2  -> ((x'-1, y'-1), N)
           3  -> ((x'-1, y'+0), W)
           4  -> ((x'-2, y'+0), N)
           5  -> ((x'-2, y'+1), E)
           6  -> ((x'-1, y'+1), N)
           7  -> ((x'-1, y'+2), W)
           8  -> ((x'-2, y'+2), S)
           9  -> ((x'-2, y'+1), W)
           10 -> ((x'-3, y'+1), N)
           11 -> ((x'-3, y'+2), W)
           12 -> ((x'-4, y'+2), S)
           13 -> ((x'-4, y'+1), W)
           14 -> ((x'-5, y'+1), S)
           15 -> ((x'-5, y'+0), E)
    S -> case r of
           0  -> ((x'+0, y'+0), S)
           1  -> ((x'+0, y'-1), W)
           2  -> ((x'-1, y'-1), N)
           3  -> ((x'-1, y'+0), W)
           4  -> ((x'-2, y'+0), N)
           5  -> ((x'-2, y'+1), E)
           6  -> ((x'-1, y'+1), N)
           7  -> ((x'-1, y'+2), W)
           8  -> ((x'-2, y'+2), N)
           9  -> ((x'-2, y'+3), E)
           10 -> ((x'-1, y'+3), S)
           11 -> ((x'-1, y'+2), E)
           12 -> ((x'+0, y'+2), N)
           13 -> ((x'+0, y'+3), E)
           14 -> ((x'+1, y'+3), N)
           15 -> ((x'+1, y'+4), W)
    W -> case r of
           0  -> ((x'+0, y'+0), N)
           1  -> ((x'+0, y'+1), E)
           2  -> ((x'+1, y'+1), S)
           3  -> ((x'+1, y'+0), E)
           4  -> ((x'+2, y'+0), S)
           5  -> ((x'+2, y'-1), W)
           6  -> ((x'+1, y'-1), S)
           7  -> ((x'+1, y'-2), E)
           8  -> ((x'+2, y'-2), N)
           9  -> ((x'+2, y'-1), E)
           10 -> ((x'+3, y'-1), S)
           11 -> ((x'+3, y'-2), E)
           12 -> ((x'+4, y'-2), N)
           13 -> ((x'+4, y'-1), E)
           14 -> ((x'+5, y'-1), N)
           15 -> ((x'+5, y'+0), W)
  where
    (n', r) = divMod n 16
    ((x, y), d) = dragon16 n'
    (x', y') = (-4*x, -4*y)

{-
(x,y) -> (x+y, y-x) -> (2y, -2x) -> (2y-2x, -2x-2y) -> (-4x, -4y)

n -> ne -> nese -> neseswse -> neseswseswnwswse
e -> se -> swse -> swnwswse -> swnwnenwswnwswse
s -> sw -> swnw -> swnwnenw -> swnwnenwnesenenw
w -> nw -> nenw -> nesenenw -> neseswsenesenenw
-}

{-
dragon_r n = dragon_d (2*n)

let n = 2^k * m, with m odd.
if k > 0, then dragon_d(2^k * m) = dragon_d(2m).

10111010100: ((14,-12),N)
10111010000: ((16,-12),S)
10111000000: ((16,-8),N)
10110000000: ((24,-8),N)
10100000000: ((32,-16),N)
10000000000: ((32,0),S)

-----------------------------
-----------------------------
10111010100: r^2 (w)    (rev)
-----------------------------
10111010000: r^4 (s)
-----------------------------
10111000000: r^6 (w)
10110000000: r^7 (nw)
10100000000: r^8 (s)    (rev)
-----------------------------
10000000000: r^10 (e)

10111010100


11000111011: r^0 (s)    (rev)
11000111010: r^1 (ne)
11000111000: r^3 (se)
11000110000: r^4 (s)
11000100000: r^5 (ne)   (rev)
11000000000: r^9 (sw)   (rev)
10000000000: r^10 (e)

11000111011
eessswwwnee
-----++++--
+-   -++ +-

10111010100
eswwwneswnn


0 -> n
1 -> ne
2 -> e
3 -> se
4 -> s
5 -> sw
6 -> w
7 -> nw
8 -> n

1-2
| |
0 3-4

4-3 0
  | |
  2-1

  0   4-3
  |     |
2-1   1-2
|     |
3-4   0
-}

dragon_ds :: [Delta]
dragon_ds = N : E : concatMap f (tail dragon_ds)
  where
    f N = [N,E]
    f E = [S,E]
    f S = [S,W]
    f W = [N,W]

dragon_rs = f dragon_ds
  where f (x:y:zs) = x : f zs

dragon_d :: Integer -> Delta
dragon_d 0 = N
dragon_d n =
  case d of
    N -> if even n then N else E
    E -> if even n then S else E
    S -> if even n then S else W
    W -> if even n then N else W
  where d = dragon_d (n `div` 2)

{-
N S S S S N S S S N N N S N S S
NSSSSNSSSNNNSNSSSNNNNSNNSNNNSNSS

dragon_r(2n) = dragon_r(n)
dragon_r(2n+1) = dragon_r(n) for odd n.
dragon_r(2n+1) = -dragon_r(n) for even n.

f(4n+0) = f(2n)
f(4n+1) = f(2n) (rev)
f(4n+2) = f(2n+2)
f(4n+3) = f(2n+2)

-}

