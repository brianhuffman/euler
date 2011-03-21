module Euler308 where

{-

Problem 308
An amazing Prime-generating Automaton
30 October 2010

A program written in the programming language Fractran consists of a
list of fractions.

The internal state of the Fractran Virtual Machine is a positive
integer, which is initially set to a seed value. Each iteration of a
Fractran program multiplies the state integer by the first fraction in
the list which will leave it an integer.

For example, one of the Fractran programs that John Horton Conway
wrote for prime-generation consists of the following 14 fractions:

  17  78  19  23  29  77  95  77   1  11  13  15   1  55
  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  91  85  51  38  33  29  23  19  17  13  11   2   7   1

Starting with the seed integer 2, successive iterations of the program
produce the sequence: 15, 825, 725, 1925, 2275, 425, ..., 68, (4), 30,
..., 136, (8), 60, ..., 544, (32), 240, ...

The powers of 2 that appear in this sequence are 2^2, 2^3, 2^5, ... It
can be shown that all the powers of 2 in this sequence have prime
exponents and that all the primes appear as exponents of powers of 2,
in proper order!

If someone uses the above Fractran program to solve Project Euler
Problem 7 (find the 10001st prime), how many iterations would be
needed until the program produces 2^(10001st prime) ?

-}


type N = Integer

program :: [(N, N)]
program =
  [(17,91),(78,85),(19,51),(23,38),(29,33),(77,29),(95,23),
   (77,19),(1,17),(11,13),(13,11),(15,2),(1,7),(55,1)]

step :: [(N, N)] -> N -> N
step [] n = error "step: program crashed"
step ((a, b) : ps) n
  | r == 0    = q * a
  | otherwise = step ps n
  where (q, r) = n `divMod` b

steps ps = iterate (step ps)

trace :: [(N, N)] -> Int -> N -> (Int, N)
trace [] i n = error "trace: program crashed"
trace ((a, b) : ps) i n
  | r == 0    = (i, q * a)
  | otherwise = trace ps (i+1) n
  where (q, r) = n `divMod` b

traces :: [(N, N)] -> N -> [(Int, N)]
traces ps n = (i, n') : traces ps n'
  where (i, n') = trace ps 0 n

{-

Write program in terms of prime factorizations:
a = 2, b = 3, c = 5, d = 7, e = 11
f = 13, g = 17, h = 19, i = 23, j = 29

Label transitions with capital letters:
A:       g     /     d f
B: ab   f      /    c   g
C:        h    /   b    g
D:         i   /  a      h
E:          j  /   b  e
F:    de       /           j
G:   c    h    /          i
H:    de       /         h
I:             /        g
J:     e       /       f
K:      f      /      e
L:  bc         /  a
M:             /     d
N:   c e       /

-}

encode 1 = []
encode n = f n codes
  where
   codes = zip [2,3,5,7,11,13,17,19,23,29] "abcdefghij"
   f n [] = ""
   f n ps@((p,c):ps') = if r == 0 then c : f n' ps else f n ps'
     where (n', r) = n `divMod` p

label i = "ABCDEFGHIJKLMN" !! i

{-

Registers a-d are unbounded, and work like program variables.

Registers e-j never exceed 1, and no two are ever set at the same
time. They seem to work like program counters.

Program code:
==================================================
0: if (a) then {L; a--; b++; c++; goto 0}
   if (d) then {M; d--; goto 0}
          else {N; c++; goto e}

e: if (b) then {E; b--; goto j}
          else {K; goto f}
j:             {F; d++; goto e}

f: if (d) then {A; d--; goto g}
          else {J; goto e}

g: if (c) then {B; a++; b++; c--; goto f}
   if (b) then {C; b--; goto h}
          else {I; goto 0}

h: if (a) then {D; a--; goto i}
          else {H; d++; goto e}
i:             {G; c++; goto h}
==================================================
Shortcuts:
==================================================
0: {a->b,c; c++; d:=0; goto e}   (LLLLMMN, a+d+1 steps)
e: {b->d; goto f}                (EFEFEFEFEFEFK, 2b+1 steps)
h: {a->c; d++; goto e}           (DGDGDGDGDGDGH, 2a+1 steps)

f: if (c&d) then {a++; b++; c--; d--; goto f}  (AB)
   if (b&d) then {b--; d--; goto h}            (AC)
   if  (d)  then {d--; goto 0}                 (AI)
            else {goto e}                      (J)

f: if (c>=d) then {a+=d; c-=d; d->b; goto e}
(ABABABABJ, 2d+1 steps)

f: if (!c&b&d) then {b--; a->c; goto e}
(ACDGDGDGDGH, 2a+3 steps)

f: if (!c&b&d) then {b--; a->c; b->d; goto f}
(ACDGDGDGDGHEFEFEFEFK, 2a+2(b-1)+4 = 2a+2b+2 steps)
==================================================
f: if (c>=d) then {a+=d; c-=d; b->d; goto f}
(ABABABABJEFEFEFEFK, 4d+2 steps)

f: if (c<d) then {a->c; b->d; d--; goto f}
(ABABACDGDGDGDGHEFEFEFK, 2a+2b+6c+2 steps)
==================================================

Special program states:

0[a,0,0,d] = 2^a*7^d = Output: integer a has greatest proper divisor d-1.

f[0,0,c,d] = 5^c*7^d*13 = Divide: begin trial division of c/d.




e[0,4,5,0] (EFEFEFEFK)
start trial division 5/4
f[0,0,5,4] (ABABABABJEFEFEFEFK, 18 steps)
f[4,0,1,4] (ABACDGDGDGDGDGHK, 16 steps)
start trial division 5/3
f[0,0,5,3] (ABABABJEFEFEFK, 14 steps)
f[3,0,2,3] (ABABACDGDGDGDGDGHEFK, 20 steps)
start trial division 5/2
f[0,0,5,2] (ABABJEFEFK, 10 steps)
f[2,0,3,2] (ABABJEFEFK, 10 steps)
f[4,0,1,2] (ABACDGDGDGDGDGHK, 16 steps)
start trial division 5/1
f[0,0,5,1] (ABJEFK, 6 steps)
f[1,0,4,1] (ABJEFK, 6 steps)
f[2,0,3,1] (ABJEFK, 6 steps)
f[3,0,2,1] (ABJEFK, 6 steps)
f[4,0,1,1] (ABJEFK, 6 steps)
f[5,0,0,1] (AI, 2 steps)
finished
0[5,0,0,0]

Cost of trial division x/d is 6*x+2*(x\d)+2

5/4 costs 6*5 + 2*1 + 2 = 34
5/3 costs 6*5 + 2*1 + 2 = 34
5/2 costs 6*5 + 2*2 + 2 = 36
5/1 costs 32






start trial division x/d
f[0,0,x,d]
(x`div`d) subtractions, each costs 4d+2 steps.
f[x-x%d, 0, x%d, d]
total cost of subtractions = (x\d)*(4d+2)
= 4*(x\d)*d + 2*(x\d)
= 4*(x - x%d) + 2*(x\d)
= 4*x - 4*(x%d) + 2*(x\d)

final reset costs = 2*(x-x%d)+6*(x%d)+2
= 2*x - 2*(x%d) + 6*(x%d) + 2
= 2*x + 4*(x%d) + 2

Grand total
= 4*x-4*(x%d)+2*(x\d) + 2*x+4*(x%d)+2
= 6*x + 2*(x\d) + 2

Cost of trial division x/d is 6*x+2*(x\d)+2


If the remainder x%d = 0, then the reset cost is reduced.
final reset cost = 2
Grand total
= 4*x-4*(x%d)+2*(x\d) + 2
= 4*x + 2*(x\d) + 2
-}

trial_division_cost c d
  | r == 0    = 4*c + 2*q + 2
  | otherwise = 6*c + 2*q + 2
  where (q, r) = divMod c d

total_trial_division_cost c = try_from (c-1)
  where
    try_from d
      | r == 0    = 4*c + 2*q + 2
      | otherwise = 6*c + 2*q + 2 + try_from (d-1)
      where (q, r) = divMod c d

-- (trial division cost, post-cleanup)
total_costs_slow c = try_from 0 (c-1)
  where
    try_from cost d
      | r == 0    = (cost + 4*c + 2*q + 2, 3*c+d+1)
      | otherwise = try_from (cost + 6*c + 2*q + 2) (d-1)
      where (q, r) = divMod c d

{-
speeding up total_costs function
total_costs (2*n) = (12*n^2 + 2, 7*n + 1)

[ total_costs (2*n) | n <- [1..] ] = [14,50,110,194,302,

-}

total_costs c
  | even c = let n = c`div`2 in (12*n^2+2, 7*n+1)
  | otherwise = total_costs_slow c

-- number of steps to get from 2 to 2^n*7^d
prob308 1 = 0
prob308 n = costs_from 0 5 2
  where
    costs_from t0 x1 c
      | c == n    = t1
      | otherwise = costs_from t1 x3 (c+1)
      where
        (x2, x3) = total_costs c
        t1 = t0 + x1 + x2

main :: IO String
main = return $ show $ prob308 104743
-- 10001st prime = 104743.

answer :: String
answer = "1539669807660924"

{-
map prob308 [1..] = [0,19,69,130,281,408,710,927,1292,1625,2375..

prob308  2000 =    10741885805
prob308  3000 =    36221238431
prob308  4000 =    85806372949
prob308  5000 =   167611799977
prob308 10000 =  1340244771627 (6 s)
prob308 15000 =  4523005078125 (15 s)
prob308 20000 = 10720696176211 (37 s)    (15 s)
prob308 25000 = 20936696110775 (47 s)
prob308 30000 = 36178097924439 (122 s)   (119 s)
prob308 40000 = 85753143585625 (426 s)

-}

