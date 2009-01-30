module Euler141 where
import SquareRoot
import Data.Array.Unboxed
import Data.List (nub)

{-
Problem 141
Investigating progressive numbers, n, which are also square.

17 February 2007

A positive integer, n, is divided by d and the quotient and remainder
are q and r respectively. In addition d, q, and r are consecutive
positive integer terms in a geometric sequence, but not necessarily in
that order.

For example, 58 divided by 6 has quotient 9 and remainder 4. It can
also be seen that 4, 6, 9 are consecutive terms in a geometric
sequence (common ratio 3/2).  We will call such numbers, n,
progressive.

Some progressive numbers, such as 9 and 10404 = 102^(2), happen to
also be perfect squares.  The sum of all progressive perfect squares
below one hundred thousand is 124657.

Find the sum of all progressive perfect squares below one trillion
(10^(12)).
-}

{-
n = k^2 = q*d + r  (r < d)
{d,q,r} are a geometric sequence
possible orderings:
  q < r < d
  r < q < d
  r < d < q

integer geometric sequences must take the form
  (aac, abc, bbc) for a < b

  (q,r,d) = (aac, abc, bbc)
  q*d + r = (aac)(bbc) + abc = (abc)^2 + abc = (abc)(abc + 1) (NOT SQUARE)

  (r,q,d) = (aac, abc, bbc)
  q*d + r = (abc)(bbc) + aac = abbbcc + aac = (ac)(bbbc + a)

  (r,d,q) = (aac, abc, bbc)
  q*d + r = (bbc)(abc) + aac = abbbcc + aac = (ac)(bbbc + a)

  (aac)(aac + 1) = aaaacc + aac < abbbcc + aac = k^2
  (aac)(aac + 1) < k^2
  (aac)^2 < k^2
  aac < k

WLOG assume r < d < q
r = aac
d = abc
q = bbc
n = k^2 = aac + abbbcc = ac (a + bbbc)

r(n-r) = rdq = ddd

(4ab^3)k^2 = (4ab^3)(a^2c + ab^3c^2)
4ab^3k^2 = 4a^3b^3c + 4a^2b^6c^2
4ab^3k^2 = (2ab^3c)^2 + 2(2ab^3c)a^2
4ab^3k^2 = (2ab^3c + a^2)^2 - a^4

3^2 = 1 + 2 * 4           (1,2,1)
102^2 = 36 + 72 * 144     (1,2,36)
130^2 = 25 + 75 * 225     (1,3,25)
312^2 = 8 + 92 * 1058     (2,23,2)
759^2 = 81 + 360 * 1600   (9,40,1)
2496^2 = 512 + 1472 * 4232  (8,23,8)
2706^2 = 1936 + 2420 * 3025 (4,5,121)
-}

{-
Testing for cubes:
mod 7: [0,1,6]
mod 9: [0,1,8]
mod 13: [0,1,5,8,12]
-}

-- all progressive numbers up to m
progressives m =
  [ n |
    a <- takeWhile (\a -> a^4 < m) [1 ..],
    let a2 = a^2,
    let b3max = (m - a2) `div` a,
    b <- takeWhile (\b -> b^3 <= b3max) [a+1 ..],
    gcd a b == 1,
    let ab3 = a * b^3,
    let ns = [ (a2 + ab3*c)*c | c <- [1 ..] ],
    n <- takeWhile (<= m) ns ]

-- perfect square progressives up to m
square_progressives m = filter is_square (progressives m)

prob141 :: Integer -> Integer
prob141 m = sum $ nub $ square_progressives (m-1)

main :: IO String
main = return $ show $ prob141 (10^12)

answer :: String
answer = "878454337159"
