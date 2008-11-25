module Euler070 where
import EulerLib
import Primes
import qualified SortedList as S
import List

------------------------------------------------------------------------------
-- 70. Investigate values of n for which φ(n) is a permutation of n.
{-
Euler's Totient function, φ(n) [sometimes called the phi function], is used to
determine the number of positive numbers less than or equal to n which are
relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than
nine and relatively prime to nine, φ(9)=6. The number 1 is considered to be
relatively prime to every positive number, so φ(1)=1.

Interestingly, φ(87109)=79180, and it can be seen that 87109 is a permutation
of 79180.

Find the value of n, 1 < n < 10^7, for which φ(n) is a permutation of n and the
ratio n/φ(n) produces a minimum.
-}

{-
Fact: φ(n) <= n.

Thus n/φ(n) >= 1.

n/φ(n) = PROD p | n. p/(p-1)
-}

type Frac = (Integer, Integer)

is_perm :: Frac -> Bool
is_perm (a, b) = sort (show a) == sort (show b)

compare_ratio :: Frac -> Frac -> Ordering
compare_ratio (a, b) (c, d) = compare (a*d) (b*c)

{-
Note: n will never be prime,
because totient p = p-1 is not a permutation of p.

Next best case is where n = p*q, both prime.

Optimally, p and q will both be near n^1/2.

We will search for p and q between n^1/3 and n^2/3.

Partial solution: (will be correct if one exists)
Search for a pair of primes p, q such that
  cube_root nmax <= p < q
  p * q <= nmax
  (n, phi(n)) = (p*q, (p-1)*(q-1))
-}

-- sortedBy compare_ratio (prime_pairs n)
prime_pairs :: Integer -> [Frac]
prime_pairs nmax = f ps0
  where
    ps0 = -- reverse $
      dropWhile (\p -> p^3 < nmax) $
      takeWhile (\p -> p^3 < nmax^2) primes
    f [] = []
    f (p:ps) =
      S.unionBy compare_ratio (f ps)
        [ (p*q, (p-1)*(q-1)) |
          q <- reverse (takeWhile (\q -> p*q < nmax) ps),
          (p + q) `mod` 9 == 1 ]

{-
optimization: the sums of digits must match
thus n == phi(n) (mod 9)

p*q == (p-1)*(q-1) (mod 9)
p*q == p*q - p - q + 1 (mod 9)
p + q == 1 (mod 9)
-}

prob70 :: Integer -> Integer
prob70 nmax = fst $ head $ filter is_perm $ prime_pairs nmax

main :: IO String
main = return $ show $ prob70 (10^7)
-- 8319823



-- complete, but slow solution
prob70_slow =
  filter is_perm $
  map (\n -> (n, totient n)) $
  [3, 5 .. 10^7]

main' = fst $ minimumBy compare_ratio prob70_slow


{-
min_ratio (a,b) (c,d) = if a*d < c*b then (a,b) else (c,d)

8319823 = 2339 * 3557
totient(8319823) = 2338 * 3556 = 8313928

totient (p^e) = p^e * (p-1)/p

let f(n) = n / totient(n)
f(p^e) = p/(p-1)

p/(p-1) = 1 + 1/(p-1)
-}
