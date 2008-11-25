module Euler155 where
import EulerLib
import qualified SortedList as S
import Ratio
import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set

{-
Problem 155
Counting Capacitor Circuits.

19 May 2007

An electric circuit uses exclusively identical capacitors of the same value C.
The capacitors can be connected in series or in parallel to form sub-units,
which can then be connected in series or in parallel with other capacitors
or other sub-units to form larger sub-units, and so on up to a final circuit.

Using this simple procedure and up to n identical capacitors, we can make
circuits having a range of different total capacitances. For example, using
up to n=3 capacitors of 60 uF each, we can obtain the following 7 distinct
total capacitance values:

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

If we denote by D(n) the number of distinct total capacitance values we can
obtain when using up to n equal-valued capacitors and the simple procedure
described above, we have: D(1)=1, D(2)=3, D(3)=7 ...

Find D(18).

Reminder: When connecting capacitors C1, C2 etc in parallel, the total
capacitance is CT = C1 + C2 +..., whereas when connecting them in series,
the overall capacitance is given by 1/CT = 1/C1 + 1/C2 + ...
-}

{-
Analysis:

The capacitance of each capacitor makes no difference to the final answer.
We might as well set the capacitance to 1.

D(1) = [1]

-}

revspan p = f []
  where
    f xs [] = (xs, [])
    f xs (y:ys) = if p y then f (y:xs) ys else (xs, y:ys)

{-
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

type Q = Ratio Int

(|+|) :: Q -> Q -> Q
x |+| y = recip (recip x + recip y)


prob155a :: Int -> Array Int [Q]
prob155a m = a
  where
    a = funArray (1, m) f
    f 1 = [1]
    f n = foldl1 S.union (map (\k -> g k (n-k)) [1 .. n`div`2])
    g m n = S.union
      (foldl1 S.union (map (\x -> map (x |+|) (a!n)) (a!m)))
      (foldl1 S.union (map (\x -> map (x +) (a!n)) (a!m)))

proper :: [Q] -> [Q]
proper = f []
  where
    f xs [] = xs
    f xs (y:ys) = if y <= 1 then f (recip y : xs) ys else S.union xs (y:ys)

prob155b :: Int -> Array Int [Q]
prob155b m = a
  where
    a = funArray (1, m) f
    f 1 = [1]
    f n = foldl1 S.union [ g i (n-i) | i <- [1 .. n`div`2] ]
    r = recip
    g i j = foldl1 S.union
      [foldl1 S.union [ [ x + y | y <- a!j ] | x <- a!i ]
      ,foldl1 S.union [ [ r x + y | y <- a!j ] | x <- a!i ]
      ,foldl1 S.union [ proper [ r (r x + r y) | y <- a!j ] | x <- a!i ]
      ,foldl1 S.union [ proper [ r (x + r y) | y <- a!j ] | x <- a!i ]
      ]

capacitor_array :: Int -> Array Int (Set Q)
capacitor_array m = a
  where
    a = funArray (1, m) f
    f 1 = Set.singleton 1
    f n = Set.unions [ g i (n-i) | i <- [1 .. n`div`2] ]
    -- precondition: i <= j
    r = recip
    g i j = canonical $ Set.unions
      [Set.unions [ Set.mapMonotonic (\y -> x + y) (a!j) | x <- xs ]
      ,Set.unions [ Set.mapMonotonic (\y -> r x + y) (a!j) | x <- xs ]
      ,Set.unions [ Set.mapMonotonic (\y -> r (x + r y)) (a!j) | x <- xs ]
      ,Set.unions [ Set.mapMonotonic (\y -> r (r x + r y)) (a!j) | x <- xs ]
      ]
      where xs = Set.toList (a!i)
    canonical s = if one then Set.insert 1 s' else s'
      where
        smin = Set.findMin s
        (lo, one, hi) = Set.splitMember 1 s
        s' = Set.union hi (Set.map r lo)

prob155 n = map length $ scanl S.union [] $ elems (prob155a n)
{-[0,1,3,7,15,35,77,179,429,1039,2525,6235,15463,38513,96231,241519,607339,1529533,3857447]-}

prob155' :: Int -> [Int]
prob155' n = map length $ scanl S.union [] $ elems (prob155b n)

prob155'' :: Int -> [Int]
prob155'' m =
  map Set.size $ scanl Set.union Set.empty $ elems $ capacitor_array m

main :: IO String
main = return $ show $ last (prob155'' 18) * 2 - 1
-- 3857447
