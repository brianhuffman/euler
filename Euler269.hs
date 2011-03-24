module Euler269 where
import qualified Data.Map as M

{---------------------------------------------------------------------

Problem 269
Polynomials with at least one integer root
19 December 2009

A root or zero of a polynomial P(x) is a solution to the equation P(x)
= 0. Define P[n] as the polynomial whose coefficients are the digits
of n. For example, P[5703](x) = 5x^3 + 7x^2 + 3.

We can see that:

    * P[n](0) is the last digit of n,
    * P[n](1) is the sum of the digits of n,
    * P[n](10) is n itself.

Define Z(k) as the number of positive integers, n, not exceeding k for
which the polynomial P[n] has at least one integer root.

It can be verified that Z(100 000) is 14696.

What is Z(10^16)?

---------------------------------------------------------------------}

-- represent polynomials as list

poly :: [Int] -> Int -> Int
poly [] x = 0
poly (c : cs) x = c + x * poly cs x

{---------------------------------------------------------------------

Since all the coefficients are non-negative, then for n and x
positive, P[n](x) is always strictly positive. Thus the roots cannot
be positive.

Also, P[n](r) == P[n](0) (mod r).
Thus, P[n](r) = 0 --> r | P[n](0).

That is, if r is a root, then r divides the least-significant digit of
n. Thus the only possible roots are [-0..-9].

P[0] has a root and P[10^k] has a root. So it doesn't matter if we sum
over n=[1..10^k] or [0..10^k-1].

---------------------------------------------------------------------}

has_root :: [Int] -> Bool
has_root cs = or [ poly cs (-r) == 0 | r <- [0..9] ]

prob269a :: Int -> Int
prob269a k = length
  [ xs | xs <- sequence (replicate k [0..9]), has_root xs ]

{---------------------------------------------------------------------

If -r is a root, that means that the polynomial P[10+r] divides P[n].
That 10+r | n is necessary, but not sufficient.

(-r) is a root of P[m*(10+r)] iff m*(10+r) can be multiplied without
carrying. That is, for each digit d in m, we must have r*d < 10;
furthermore, for each pair of adjacent digits (a,b) in m, we must have
a+r*b < 10.

Permissible digit pairs in m:
root | digits, disallowed pairings
--------------------------
 -0  | 0-9 | none
 -1  | 0-9 | ..19..28..37..46.. any pair that add to 10 or more
 -2  | 0-4 | ..44..34..24..43..
 -3  | 0-3 | ..33..23..13..
 -4  | 0-2 | ..22..
 -5  | 0-1 | none
 -6  | 0-1 | none
 -7  | 0-1 | none
 -8  | 0-1 | none
 -9  | 0-1 | ..11..

For each root, we can make a DFA where states are digits in m, and
edges are labeled with digits from n.

Root = -9:
0->0: 0
0->1: 1
1->0: 9

Root = -8    Root = -7    Root = -6    Root = -5
0->0: 0      0->0: 0      0->0: 0      0->0: 0
0->1: 1      0->1: 1      0->1: 1      0->1: 1
1->1: 8      1->1: 7      1->1: 6      1->1: 5
1->1: 9      1->1: 8      1->1: 7      1->1: 6

Root = -4
0->0: 0   1->0: 4   2->0: 8
0->1: 1   1->1: 5   2->1: 9
0->2: 2   1->2: 6

Root = -3
0->0: 0   1->0: 3   2->0: 6   3->0: 9
0->1: 1   1->1: 4   2->1: 7
0->2: 2   1->2: 5   2->2: 8
0->3: 3   1->3: 6   2->3: 9

Root = -2
0->0: 0   1->0: 2   2->0: 4   3->0: 6   4->0: 8
0->1: 1   1->1: 3   2->1: 5   3->1: 7   4->1: 9
0->2: 2   1->2: 4   2->2: 6   3->2: 8
0->3: 3   1->3: 5   2->3: 7   3->3: 9
0->4: 4   1->4: 6   2->4: 8

Root = -1:
...

Root = -0:
...

If we do a naive product DFA construction on all 10 DFAs,
the total state size will be 10*10*5*4*3*2*2*2*2*2 = 192000.

============================================================

Possible roots, by cases on last digit of n:
--------------------------------------------
 0 | Always has root r = 0.
 1 | -1
 2 | -1 or -2
 3 | -1 or -3
 4 | -1 or -2 or -4
 5 | -1 or -5
 6 | -1 or -2 or -3 or -6
 7 | -1 or -7
 8 | -1 or -2 or -4 or -8
 9 | -1 or -3 or -9

P[11] and P[12] both divide P[n] <--> P[132] divides P[n].
P[11] and P[13] both divide P[n] <--> P[143] divides P[n].
P[11] and P[14] both divide P[n] <--> P[154] divides P[n].
P[11] and P[15] both divide P[n] <--> P[165] divides P[n].
P[11] and P[16] both divide P[n] <--> P[176] divides P[n].
P[11] and P[17] both divide P[n] <--> P[187] divides P[n].
P[11] and P[18] both divide P[n] <--> P[198] divides P[n].
P[11] and P[19] both divide P[n] <--> P[1,10,9] divides P[n] -- impossible!

P[12] and P[14] both divide P[n] <--> P[168] divides P[n].
P[12] and P[16] both divide P[n] <--> P[1,8,12] divides P[n] -- impossible!
P[12] and P[18] both divide P[n] <--> P[1,10,16] divides P[n] -- impossible!

P[13] and P[16] both divide P[n] <--> P[1,9,18] divides P[n] -- impossible!
P[13] and P[19] both divide P[n] <--> P[1,12,27] divides P[n] -- impossible!

P[14] and P[18] both divide P[n] <--> P[1,12,32] divides P[n] -- impossible!

P[11] and P[12] and P[13] all divide P[n] <--> P[1716] divides P[n]
P[11] and P[12] and P[14] all divide P[n] <--> overflow!

Revised table of possible roots, by cases on last digit of n:
--------------------------------------------
 0 | Always has root r = 0.
 1 | p(1)
 2 | p(1) + p(2) - p(1,2)
 3 | p(1) + p(3) - p(1,3)
 4 | p(1) + p(2) + p(4) - p(1,2) - p(1,4) - p(2,4)
 5 | p(1) + p(5) - p(1,5)
 6 | p(1) + p(2) + p(3) + p(6) - p(1,2) - p(1,3) - p(1,6) + p(1,2,3)
 7 | p(1) + p(7) - p(1,7)
 8 | p(1) + p(2) + p(4) + p(8) - p(1,2) - p(1,4) - p(2,4)
 9 | p(1) + p(3) + p(9) - p(1,3)

======================================================================

 0 is a root iff 10 | n.
-1 is a root --> 11 | n  (not sufficient! 11|209, but P[209] has no root)
-2 is a root --> n == [2,4,6,8] (mod 10)
-3 is a root --> n == [3,6,9] (mod 10)
-4 is a root --> n == 4 or 8 (mod 10)
-5 is a root --> n == 5 (mod 10)
-6 is a root --> n == 6 (mod 10)
-7 is a root --> n == 7 (mod 10)
-8 is a root --> n == 8 (mod 10)
-9 is a root --> n <- [19, 1919, 191919, 19191919, 1919191919, ...

Regular expressions:
-9 is a root: n = (19|0)*
-8 is a root: n = (19*8|0)*
-7 is a root: n = (18*7|0)*
-6 is a root: n = (17*6|0)*
-5 is a root: n = (16*5|0)*
-4 is a root: ...
-3 is a root: 13, 26, 39 ...
-2 is a root: n = 
-1 is a root: n = 
-0 is a root: n = (1|2|3|4|5|6|7|8|9|0)*0

---------------------------------------------------------------------}

data DFA s a = DFA s (s -> a -> s) (s -> Bool)

unionDFA :: DFA s a -> DFA t a -> DFA (s, t) a
unionDFA (DFA s0 s' sf) (DFA t0 t' tf) =
  DFA (s0, t0) (\(s, t) x -> (s' s x, t' t x)) (\(s, t) -> sf s || tf t)

infixr 8 +++
(+++) = unionDFA

testDFA :: DFA s a -> [a] -> Bool
testDFA (DFA start next final) xs = final (foldl next start xs)

----------------------------------------------------------------------
-- DFAs for recognizing polynomials with root r; n given in lsb order

-- r = 0:
rdfa0 :: DFA Int Int
rdfa0 = DFA 0 next (<2)
  where
    next 1 x = 1 -- accept
    next 2 x = 2 -- reject
    next 0 0 = 1 -- accept
    next 0 x = 2 -- reject

rdfa1 :: DFA Int Int
rdfa1 = DFA 0 d (==0)
  where d s x = x - s

rdfa :: Int -> DFA (Maybe Int) Int
rdfa r = DFA (Just 0) d (== Just 0)
  where
    d Nothing x = Nothing
    d (Just s) x
      | m == 0 = Just t
      | otherwise = Nothing
      where (t, m) = divMod (x-s) r

super_dfa =
  rdfa0 +++ rdfa1 +++ rdfa 2 +++ rdfa 3 +++ rdfa 4 +++
  rdfa 5 +++ rdfa 6 +++ rdfa 7 +++ rdfa 8 +++ rdfa 9

prob269b :: Int -> Int
prob269b k = length
  [ xs | xs <- sequence (replicate k [0..9]), testDFA super_dfa xs ]

{---------------------------------------------------------------------
testing the DFA:
and [ has_root xs == testDFA super_dfa xs |
      xs <- sequence (replicate 5 [0..9]) ]
---------------------------------------------------------------------}

type Bag a = M.Map a Integer

mapBag :: (Ord b) => (a -> b) -> Bag a -> Bag b
mapBag = M.mapKeysWith (+)

unionsBag :: (Ord a) => [Bag a] -> Bag a
unionsBag = M.unionsWith (+)

emptyBag :: Bag a
emptyBag = M.empty

insertBag :: (Ord a) => a -> Bag a -> Bag a
insertBag x = M.insert x 1

countBag :: (s -> Bool) -> Bag s -> Integer
countBag p = M.foldWithKey f 0
  where f x m t = if p x then t+m else t

----------------------------------------------------------------------

stepallDFA :: (Ord s) => DFA s a -> [[a]] -> Integer
stepallDFA (DFA start next final) xss = countBag final (f b0 xss)
  where
    b0 = insertBag start emptyBag
    f b [] = b
    f b (xs : xss) =
      f (unionsBag [ mapBag (\s -> next s x) b | x <- xs ]) xss

prob269 :: Int -> Integer
prob269 k = stepallDFA super_dfa (replicate k [0..9])

main :: IO String
main = return $ show $ prob269 16

answer :: String
answer = "1311109198529286"
