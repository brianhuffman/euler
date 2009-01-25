module Euler229 where
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import EulerLib (square_root)
import Data.Array.Unboxed

{-
Problem 229
24 January 2009

Consider the number 3600. It is very special, because

      3600 = 48^(2) +     36^(2)

      3600 = 20^(2) + 2×40^(2)

      3600 = 30^(2) + 3×30^(2)

      3600 = 45^(2) + 7×15^(2).

Similarly, we find that 88201 = 99^(2) + 280^(2) = 287^(2) + 2×54^(2)
= 283^(2) + 3×52^(2) = 197^(2) + 7×84^(2).

In 1747, Euler proved which numbers are representable as a sum of two
squares. We are interested in the numbers n which admit
representations of all of the following four types:

      n = a_(1)^(2) +   b_(1)^(2)

      n = a_(2)^(2) + 2 b_(2)^(2)

      n = a_(3)^(2) + 3 b_(3)^(2)

      n = a_(7)^(2) + 7 b_(7)^(2),

where the a_(k) and b_(k) are positive integers.

There are 75373 such numbers that do not exceed 10^(7).
How many such numbers are there that do not exceed 2×10^(9)?
-}


{-

If x = a^2 + b^2, and y = c^2 + d^2,
then xy = (ac - bd)^2 + (ad + bc)^2.

Let x = a^2 + pb^2
Let y = c^2 + pd^2
Then xy = (ac + pbd)^2 + p(ad - bc)^2
Also xy = (ac - pbd)^2 + p(ad + bc)^2

Therefore the set of such numbers is closed w.r.t. multiplication.

----------------------------

Let x = a^2 + pb^2
Then xk^2 = (ak)^2 + p(bk)^2

Therefore the set of such numbers is closed w.r.t. scaling by squares.

----------------------------

a^2 +  b^2 == 0,1,2,4,5 (mod 8)
a^2 + 2b^2 == 0,1,2,3,4,6 (mod 8)
a^2 + 3b^2 == 0,1,3,4,5,7 (mod 8)
a^2 + 7b^2 == 0,1,3,4,5,7 (mod 8)

such numbers == 0,1,4 (mod 8)
such numbers == 0,1,4,9 (mod 12)
such numbers == 0,1,4,8,9,16,21,25 (mod 28)

such numbers == 0,1 (mod 3)
such numbers == 0,1,4,7 (mod 9)
such numbers == 0,1,4,9 (mod 16)

For such a number to be a multiple of 4:
  a1^2 +   b1^2 == 0 (mod 4)
  a2^2 + 2*b2^2 == 0 (mod 4)
  a3^2 + 3*b3^2 == 0 (mod 4)
  a7^2 + 7*b7^2 == 0 (mod 4)

  a1, b1 both even.
  a2, b2 both even.
  a3, b3 both even, or both odd.
  a7, b7 both even, or both odd.


For such a number to be a multiple of 4 + 1:
  a1^2 +   b1^2 == 1 (mod 4)
  a2^2 + 2*b2^2 == 1 (mod 4)
  a3^2 + 3*b3^2 == 1 (mod 4)
  a7^2 + 7*b7^2 == 1 (mod 4)

  a1 odd, b1 even; or vice-versa.
  a2, b2 both even.
  a3, b3 both even, or both odd.
  a7, b7 both even, or both odd.

-}

ok :: Int -> Bool
ok n = ok2 && ok3 && ok7
  where
    ok2 = (n `mod` 16) `elem` [0,1,4,9]
    ok3 = (n `mod` 9) `elem` [0,1,4,7]
    -- ok3 = (n `mod` 12) `elem` [0,1,4,9]
    ok7 = (n `mod` 28) `elem` [0,1,4,8,9,16,21,25]

such_numbers :: Int -> IntSet
such_numbers m = foldl1 Set.intersection [set1, set 2, set 3, set 7]
  where
    -- r = square_root m
    squares = map (^2) [1 ..]
    arr1 :: UArray Int Bool
    arr1 = accumArray (const id) False (1, m)
      [ (a2 + b2, True) |
        a2 <- takeWhile (<= m) squares,
        let b2max = min a2 (m - a2),
        b2 <- takeWhile (<= b2max) squares ]
    set1 = Set.fromAscList [ n | n <- [1 .. m], arr1 ! n, ok n ]
    arr :: Int -> UArray Int Bool
    arr k = accumArray (const id) False (1, m)
      [ (a2 + k*b2, True) |
        a2 <- takeWhile (<= m) squares,
        let b2max = (m - a2) `div` k,
        b2 <- takeWhile (<= b2max) squares ]
    set k = Set.fromAscList [ n | n <- [1 .. m], a ! n, ok n ]
      where a = arr k
    set' k = Set.unions
      [ Set.fromAscList $ filter ok $ takeWhile (<= m)
          [ a2 + k*b^2 | a2 <- squares ] | b <- [1 .. square_root (m`div`k)] ]

count_such_numbers :: Int -> Int
count_such_numbers m = length ns
  where
    -- r = square_root m
    squares = map (^2) [1 ..]
    ok :: Int -> Bool
    ok n = (n `mod` 16) `elem` [0,1,4,9]
    scale :: Int -> Int
    scale n =
      case n `mod` 16 of
        0 -> n `div` 4
        _ -> n `div` 4 + 1
    arr1 :: UArray Int Bool
    arr1 = accumArray (const id) False (1, scale m)
      [ (scale n, True) |
        a2 <- takeWhile (<= m) squares,
        let b2max = min a2 (m - a2),
        b2 <- takeWhile (<= b2max) squares,
        let n = a2 + b2, ok n ]
    ns = [ n | n <- [1 .. scale m], arr1 ! n, arr2 ! n, arr3 ! n, arr7 ! n ]
    arr :: Int -> UArray Int Bool
    arr k = accumArray (const id) False (1, scale m)
      [ (scale n, True) |
        a2 <- takeWhile (<= m) squares,
        let b2max = (m - a2) `div` k,
        b2 <- takeWhile (<= b2max) squares,
        let n = a2 + k*b2, ok n ]
    arr2 = arr 2
    arr3 = arr 3
    arr7 = arr 7

--list_such_numbers m = Set.toAscList (such_numbers m)

main :: IO String
main = return $ show $ count_such_numbers (2*10^9)
-- 11325263

{-

Let 2x = a^2 + b^2.
Then a-b is even.
Then x = ((a+b)/2)^2 + ((a-b)/2)^2.

Let 2x = a^2 + 2b^2.
Then a is even; let a = 2c.
Then x = b^2 + 2c^2.

Let 4x = a^2 + 3b^2.
Then a-b is even.

d = (a-b)/4
c = (a+3b)/4

(5,7) -> (4,3)
(7,9) -> (5,4)

Let x = a^2 + 3b^2
Then 4x = (a + 3b)^2 + 3(a - b)^2
Also 4x = (a - 3b)^2 + 3(a + b)^2

Let x = c^2 + 3d^2
Then 4x = (c + 3d)^2 + 3(c - d)^2

a^2 + 3b^2 == 0 (for both even)
a^2 + 3b^2 == 1 (odd a, even b)
a^2 + 3b^2 == 3 (even a, odd b)
a^2 + 3b^2 == 0 (for both odd)


Let x = a^2 + 3b^2
Let y = c^2 + 3d^2
Then xy = (ac + 3bd)^2 + 3(ad - bc)^2


a^2 + b^2 == 0 (mod 4)  (for both even)
a^2 + b^2 == 1 (mod 4)  (for one even, one odd)
a^2 + b^2 == 2 (mod 4)  (for both odd)

----------------------------

Prime factors p | a^2 + b^2:
  2, or p == 1 (mod 4)

Primes factors p | a^2 + 2b^2:
  2, or p == 1,3 (mod 8)

Primes factors p | a^2 + 3b^2:
  2, or p == 1,7,9 (mod 12)

Primes factors p | a^2 + 7b^2:
  2, or p == 1,7,9,11,15,23,25 (mod 28)


[2,5,13,17,29,37,41,53,61,73,89,97,101,109,113,137,149,157,173,181,193]
[3,11,17,19,41,43,59,67,73,83,89,97,107,113,131,137,139,163,179,193]
[7,13,19,31,37,43,61,67,73,79,97,103,109,127,139,151,157,163,181,193]
[7,11,23,29,37,43,53,67,71,79,107,109,113,127,137,149,151,163,179,191,193]

 5^(2) = 3^(2) + 4^(2)
13^(2) = 5^(2) + 12^(2)
17^(2) = 8^(2) + 15^(2)

 3^(2) = 1^(2) + 2*2^(2)
11^(2) = 7^(2) + 2*6^(2)
17^(2) = 1^(2) + 2*12^(2)

 2^(2) =  1^(2) + 3*1^(2)
13^(2) = 11^(2) + 3*4^(2)

 4^(2) = 3^(2) + 7*1^(2)

60 = 2 2 3 5
68 = 2 2 17
110 = 2 5 11
111 = 3 37
143 = 11 13
156 = 2 2 3 13
164 = 2 2 41
174 = 2 3 29
204 = 2 2 3 17
215 = 5 43
226 = 2 113
274 = 2 137
286
292
300
312

-}