module Euler273 where
import SquareRoot
import Primes

{---------------------------------------------------------------------
Problem 273
Sum of Squares

09 January 2010

Consider equations of the form: a^(2) + b^(2) = N, 0 ≤ a ≤ b, a, b and
N integer.

For N=65 there are two solutions:

a=1, b=8 and a=4, b=7.

We call S(N) the sum of the values of a of all solutions of a^(2) +
b^(2) = N, 0 ≤ a ≤ b, a, b and N integer.

Thus S(65)=1+4=5.

Find ∑S(N), for all squarefree N only divisible by primes of the form
4k+1 with 4k+1 < 150.

---------------------------------------------------------------------}

type Z = Integer

good_ps :: [Z]
good_ps = [ p | p <- takeWhile (<150) primes, p `mod` 4 == 1 ]

products :: [Z] -> [Z]
products [] = [1]
products (x : xs) = map (*x) ys ++ ys
  where ys = products xs

good_ns :: [Z]
good_ns = products good_ps

{---------------------------------------------------------------------

The allowable factors are:
 [5,13,17,29,37,41,53,61,73,89,97,101,109,113,137,149]

(There are 16 of them. Thus there are 2^16 possible values of N.)

Each prime p == 1 (mod 4) can be written as a sum of squares in 1 way.
(Here <a, b> means a^2 + b^2.)

5 = <1,2>
13 = <2,3>
17 = <1,4>
29 = <2,5>
37 = <1,6>
41 = <4,5>
53 = <2,7>
61 = <5,6>
73 = <3,8>
89 = <5,8>
97 = <4,9>
101 = <1,10>
109 = <3,10>
113 = <7,8>
137 = <4,11>
149 = <7,10>

<1,2>*<2,3> = <1,8> = <4,7>
<1,2>*<2,5> = <1,12> = <8,9>
<1,3>*<2,5> = <1,17> = <11,13>
<1,4>*<2,5> = <3,22> = <13,18>
<2,3>*<2,5> = <4,19> = <11,16>

<a,b>*<c,d> = <ad-bc, ac+bd> = <bd-ac, ad+bc>

---------------------------------------------------------------------}

type Pair = (Z, Z)

brute_force :: Z -> [Pair]
brute_force n =
  [ (a, b) | 
    a <- [1 .. square_root (n `div` 2)],
    let (b, r) = square_root_aux (n - a^2),
    r == 0
  ]


pairs :: [Pair]
pairs =
  [(1,2),(2,3),(1,4),(2,5),(1,6),(4,5),(2,7),(5,6),
   (3,8),(5,8),(4,9),(1,10),(3,10),(7,8),(4,11),(7,10)]

orient :: Pair -> Pair
orient p@(x,y) = if x <= y then p else (y,x)

combine :: Pair -> Pair -> [Pair]
combine (a,b) (c,d) =
  [orient (abs(a*d-b*c), a*c+b*d), orient (abs(b*d-a*c), a*d+b*c)]

combinations :: [Pair] -> [Pair]
combinations [] = []
combinations (p : ps) = p : concatMap (combine p) cs ++ cs
  where cs = combinations ps

prob273 :: [Pair] -> Integer
prob273 = sum . map fst . combinations

main :: IO String
main = return $ show $ prob273 pairs

answer :: String
answer = "2032447591196869022"
