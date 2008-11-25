module Euler131 where
import Primes
import qualified SortedList as S

------------------------------------------------------------------------------
-- 131. Determining primes, p, for which n^3 + n^2*p is a perfect cube.
{-
There are some prime values, p, for which there exists a positive integer, n,
such that the expression n^3 + n^2*p is a perfect cube.

For example, when p = 19, 8^3 + 8^2*19 = 12^3.

What is perhaps most surprising is that for each prime with this property the
value of n is unique, and there are only four such primes below one-hundred.

How many primes below one million have this remarkable property?
-}

{-
Find prime p such that there exist integers n, m, with
  n^3 + n^2*p = m^3

n^2 * (n + p) = m^3

Show: n and (n+p) are both perfect cubes.
Proof by cases.
  Case (p divides n):
    Obtain k where n = k*p.
    n^3 + n^2*p = m^3
    (k*p)^3 + (k*p)^2*p = m^3
    k^3*p^3 + k^2*p^2*p = m^3
    k^3*p^3 + k^2*p^3 = m^3
    (k^3 + k^2) * p^3 = m^3
    Thus p | m^3, and p | m.
    Obtain l where m = l*p.
    (k^3 + k^2) * p^3 = (l*p)^3
    (k^3 + k^2) * p^3 = l^3*p^3
    k^3 + k^2 = l^3
    This is a contradiction, because
      k^3 < k^3 + k^2 < (k+1)^3.
    I.e. k^3 + k^2 is strictly between two adjacent cubes;
    Therefore it cannot itself be a perfect cube.

  Case (p does not divide n):
    Then (n^2) and (n+p) are coprime.
    (Any prime factor q | n^2 --> q | n --> q /| n+p.)
    Then (n^2) and (n+p) are perfect cubes.
    Then n and (n+p) are perfect cubes.

Thus p must be a difference of two cubes.

Since (a-b) is a factor of a^3 - b^3, we know that
  p must be a difference of two adjacent cubes.

Thus p must be of the form 3*a^2 + 3*a + 1.
-}

cube_diffs = [ 3*n*n + 3*n + 1 | n <- [1 ..] ]

prob131 m = length $
  filter is_prime $
  takeWhile (< m) cube_diffs

main :: IO String
main = return $ show $ prob131 (10^6)
-- 173
