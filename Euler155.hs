module Euler155 where
import EulerLib (funArray, fibonacci)
import Data.Array.IArray
import Data.Array.Unboxed

{-
Problem 155
Counting Capacitor Circuits.

19 May 2007

An electric circuit uses exclusively identical capacitors of the same
value C.  The capacitors can be connected in series or in parallel to
form sub-units, which can then be connected in series or in parallel
with other capacitors or other sub-units to form larger sub-units, and
so on up to a final circuit.

Using this simple procedure and up to n identical capacitors, we can
make circuits having a range of different total capacitances. For
example, using up to n=3 capacitors of 60 uF each, we can obtain the
following 7 distinct total capacitance values:

   +--||--+
   |      |
---+--||--+--- : 180 uF
   |      |
   +--||--+

   +--||--+
---+      +--- : 120 uF
   +--||--+

  +-||--||-+
--+        +-- : 90 uF
  +---||---+

------||------ : 60 uF

      +-||-+
--||--+    +-- : 40 uF
      +-||-+

----||--||---- : 30 uF

--||--||--||-- : 20 uF

If we denote by D(n) the number of distinct total capacitance values
we can obtain when using up to n equal-valued capacitors and the
simple procedure described above, we have: D(1)=1, D(2)=3, D(3)=7 ...

Find D(18).

Reminder: When connecting capacitors C1, C2 etc in parallel, the total
capacitance is CT = C1 + C2 +..., whereas when connecting them in
series, the overall capacitance is given by 1/CT = 1/C1 + 1/C2 + ...
-}

{-
Analysis:

The capacitance of each capacitor makes no difference to the final answer.
We might as well set the capacitance to 1.

1: 1
2: 1/2, 2/1
3: 1/3, 2/3, 3/2, 3/1
4: 1/4, 2/5, 3/5, 3/4, 1/1, 4/3, 5/3, 5/2, 4/1
5: 1/5, 2/7, 3/8, 3/7, 1/2, 4/7, 5/8, 5/7, 4/5, 5/6, 6/7,
   7/6, 6/5, 5/4, 7/5, 8/5, 7/4, 2/1, 7/3, 8/3, 7/2, 5/1

Observation:
The maximum numerator/denominator for n capacitors = fib(n+1).
The maximum numerator/denominator for 18 capacitors = 4181.

F(a+b+1) = F(a) * F(b) + F(a+1) * F(b+1)

-}

type Frac = (Int, Int)

(+++) :: Frac -> Frac -> Frac
(a,b) +++ (c,d) = (a*d + b*c, b*d)

(|||) :: Frac -> Frac -> Frac
(a,b) ||| (c,d) = (a*c, a*d + b*c)

reduce :: Frac -> Frac
reduce x@(a,b) = if d == 1 then x else (a`div`d, b`div`d)
  where d = gcd a b

type FracSet = UArray Frac Bool

set2list :: FracSet -> [Frac]
set2list arr = [ x | (x, True) <- assocs arr ]

capacitor_sets :: Int -> Array Int FracSet
capacitor_sets m = a
  where
    a = funArray (1, m) f
    f 1 = listArray ((1,1),(1,1)) [True]
    f n = accumArray (const id) False ((1,1),(s,s))
      [ (z, True) |
        i <- [1 .. n`div`2],
        x <- set2list (a!i),
        y <- set2list (a!(n-i)),
        z <- [x +++ y, x ||| y] ]
      where s = fibonacci (n + 1)

capacitor_set :: Int -> FracSet
capacitor_set m = set3
  where
    (set1 : sets) = reverse $ elems $ capacitor_sets m
    set2 = accum (const id) set1
      [ (x, True) | s <- sets, x <- set2list s ]
    set3 = accumArray (const id) False (bounds set2)
      [ (reduce x, True) | x <- set2list set2 ]

prob155 :: Int -> Int
prob155 n = length $ set2list $ capacitor_set n

{-

    A153588  A048211
 n     D(n)     C(n)  F(n+1)^2
------------------------------
 1        1        1         1
 2        3        2         4
 3        7        4         9
 4       15        9        25
 5       35       22        64
 6       77       53       169
 7      179      131       441
 8      429      337      1156
 9     1039      869      3025
10     2525     2213      7921
11     6235     5691     20736
12    15463    14517     54289
13    38513    37017    142129
14    96231    93731    372100
15   241519   237465    974169
16   607339   601093   2550409
17  1529533  1519815   6677056
18  3857447  3842575  17480761
-}

main :: IO String
main = return $ show $ prob155 18

answer :: String
answer = "3857447"
