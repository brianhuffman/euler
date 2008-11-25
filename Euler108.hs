module Euler108 where
import EulerLib
import Primes

------------------------------------------------------------------------------
-- 108. Solving the Diophantine equation 1/x + 1/y = 1/n.
{-
In the following equation x, y, and n are positive integers.

1/x + 1/y = 1/n

For n = 4 there are exactly three distinct solutions:

1/5 + 1/20 = 1/4
1/6 + 1/12 = 1/4
1/8 + 1/8 = 1/4

What is the least value of n for which the number of distinct solutions
exceeds one-thousand?

NOTE: This problem is an easier version of problem 110; it is strongly
advised that you solve this one first.
-}

{-
Analysis:

1/x + 1/y = 1/n,  with x <= y
  iff
x*y = n*(x+y)

Let x = n+d, 0 < d <= n.
Let y = n+e, 0 < e <= n.

x*y = n*(x+y)
(n+d)*(n+e) = n*((n+d)+(n+e))
n*n + n*d + n*e + d*e = 2*n*n + n*d + n*e
d*e = n*n

Thus all solutions to 1/x + 1/y = 1/n have the form
  1/(n+d) + 1/(n+e)
For d*e = n*n.

Swapping the order does not count as a distinct solution.
Therefore, only consider solutions with d <= e, i.e. d <= n.

So number of solutions = number of divisors of n^2 <= n.

----------------------------------------------------------

Number of divisors of n^2,
where n = p1^e1 * p2^e2 * ... * pk^ek
is (2*e1+1) * (2*e2+1) * ... * (2*ek+1).

That is, the number of divisors depends only on the
set of prime exponents in the prime factorization.

If the prime exponents of n are not a decreasing sequence,
then by permuting primes we can construct a smaller n'
with the same number of divisors. Thus the optimal n has
a decreasing sequence of prime exponents.

more than 1000 solutions <--> n^2 has more than 1999 divisors
-}

square_divisors :: [Int] -> Int
square_divisors es = product [ 2*e+1 | e <- es ]

-- Each list es in the result should satisfy
-- (1) decreasing es
-- (2) m < square_divisors es
-- Also, no prefix of any list should satisfy (2).

exponent_seqs m = seqs m (emax m)
  where
    -- emax m = least e such that 2*e + 1 > m.
    emax m = (m+1) `div` 2
    seqs 0 _ = return []
    seqs m e1 = do
      e <- [1 .. min (emax m) e1]
      let d = 2*e + 1
      -- Least m' such that for all x > m', d*x > m
      let m' = m `div` d
      es <- seqs m' e
      return (e : es)

prime_exp :: [Int] -> Integer
prime_exp = product . zipWith (^) primes

-- fst (prob108 n) = minimum $ map prime_exp $ exponent_seqs (2*n)

type State = (Integer, Integer)
-- (n, number of divisors of n^2)

prob108 :: Integer -> State
prob108 n = find_least primes n (1, 1)
  where
    extend :: Integer -> Integer -> State -> State
    extend p e (t, nf) = (t*p^e, nf*(2*e+1))

    done :: State -> Bool
    done (t, nf) = nf > 2*n

    find_least :: [Integer] -> Integer -> State -> State
    find_least (p:ps) emax s
      | done s = s
      | otherwise = try_exps (p:ps) emax best s 2
          where best = find_least ps 1 (extend p 1 s)

    try_exps :: [Integer] -> Integer -> State -> State -> Integer -> State
    try_exps (p:ps) emax best s e
      | e > emax = best
      | best <= extend p e s = best
      | otherwise = try_exps (p:ps) emax best' s (e+1)
          where best' = min best (find_least ps e (extend p e s))

main :: IO String
main = return $ show $ fst $ prob108 (10^3)
-- 180180
