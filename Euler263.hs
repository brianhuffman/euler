module Euler263 where
import Primes
import Permutation
import Data.List
import qualified SortedList as S

{---------------------------------------------------------------------
Problem 263
An Engineer's Dream Come True

07 November 2009

Consider the number 6. The divisors of 6 are: 1,2,3 and 6.

Every number from 1 up to and including 6 can be written as a sum of
distinct divisors of 6:

1=1, 2=2, 3=1+2, 4=1+3, 5=2+3, 6=6.

A number n is called a practical number if every number from 1 up to
and including n can be expressed as a sum of distinct divisors of n.

A pair of consecutive prime numbers with a difference of six is called
a sexy pair (since "sex" is the Latin word for "six"). The first sexy
pair is (23, 29).

We may occasionally find a triple-pair, which means three consecutive
sexy prime pairs, such that the second member of each pair is the
first member of the next pair.

We shall call a number n such that :

    * (n-9, n-3), (n-3,n+3), (n+3, n+9) form a triple-pair, and
    * the numbers n-8, n-4, n, n+4 and n+8 are all practical, 

an engineers’ paradise.

Find the sum of the first four engineers’ paradises.

---------------------------------------------------------------------}

subset_sums :: [Int] -> [Int]
subset_sums [] = [0]
subset_sums (x : xs) = S.union ys (map (x+) ys)
  where ys = subset_sums xs

divisor_sums :: Int -> [Int]
divisor_sums n = subset_sums (list_divisors n)

practical_slow :: Int -> Bool
practical_slow n = [0 .. n] `isPrefixOf` divisor_sums n

practicality :: Int -> Int
practicality n =
  length (takeWhile id (zipWith (==) [0..] (divisor_sums n))) - 1

repunit :: Int -> Int -> Int
repunit b n = f 0 n
  where
    f x 0 = x
    f x n = f (x*b+1) (n-1)

practical :: Int -> Bool
practical n = f 1 (prime_factorization n)
  where
    f m [] = True
    f m ((p,e) : pf) = p <= m+1 && f (m * repunit p (e+1)) pf

{---------------------------------------------------------------------
-- Properties of Practical Numbers

For practical numbers, every value from 1 to sum_divisors n is
expressible as a sum of disjoint factors.

----------------------------------------------------------------------

An odd number > 1 cannot be practical.  There is no way for factors to
sum to 2.

Thus a == 1 (mod 2) is not practical.

A number with one 2, no 3s, and at least one larger factor in its
prime factorization cannot be practical.  Factors: 1, 2, (next factor
is at least 5).  There is no way for factors to sum to 4.

Thus a == 2 or 10 (mod 12) is not practical.

A number with two 2s, but no 3, 5 or 7 in its prime factorization
cannot be practical.  1, 2, 1+2=3, 4, 1+4=5, 2+4=6, 1+2+4=7, (next
factor is at least 11).  There is no way for factors to sum to 8.

Thus one of the following must hold: 8|a or 3|a or 5|a or 7|a.

A number with three 2s, but no 3, 5, 7, 11 or 13 in its prime
factorization cannot be practical.  There is no way for factors to sum
to 16.

Thus one of the following must be a factor: 16, 3, 5, 7, 11, or 13.

----------------------------------------------------------------------

* the numbers n-8, n-4, n, n+4 and n+8 are all practical, 

None of these can be congruent to 2 or 10 (mod 12).

This rules out n == 2,6,10 (mod 12), i.e. n == 2 (mod 4) is ruled out.

Therefore, we must have n == 0 (mod 4).

* the numbers n-9, n-3, n+3, n+9 are all prime.

Primality requires:
* n == 1,2 (mod 3).
* n == 0 (mod 5).
* n == 0,1,6 (mod 7)

For these moduli, we have these possible values for
n-8, n-4, n, n+4 and n+8:

(mod 8)
  0  4  0  4  0
  4  0  4  0  4

(mod 3)
  2  0  1  2  0
  0  1  2  0  1

(mod 5)
  2  1  0  4  3

(mod 7)
  5  1  6  2  0
  6  4  0  5  1
  0  5  1  6  2

Each position must have a zero in one of these moduli.

Possible combination 1:
  4  0  4  0  4  (mod 8)
  2  0  1  2  0  (mod 3)
  2  1  0  4  3  (mod 5)
  0  5  1  6  2  (mod 7)
  n == 820 (mod 840)

Possible combination 2:
  4  0  4  0  4  (mod 8)
  0  1  2  0  1  (mod 3)
  2  1  0  4  3  (mod 5)
  5  1  6  2  0  (mod 7)
  n == 20 (mod 840)

---------------------------------------------------------------------}

candidates :: () -> [Int]
candidates () =
  [ n |
    k <- [0, 840 ..],
    n <- [k+20, k+820],
    is_prime (n-9),
    is_prime (n-3),
    is_prime (n+3),
    is_prime (n+9),
    not (is_prime (n-7)),
    not (is_prime (n-5)),
    not (is_prime (n-1)),
    not (is_prime (n+1)),
    not (is_prime (n+5)),
    not (is_prime (n+7))
  ]

paradises :: () -> [Int]
paradises () =
  [ n |
    k <- [0, 840 ..],
    n <- [k+20, k+820],
    is_prime (n-9),
    is_prime (n-3),
    is_prime (n+3),
    is_prime (n+9),
    not (is_prime (n-7)),
    not (is_prime (n-5)),
    not (is_prime (n-1)),
    not (is_prime (n+1)),
    not (is_prime (n+5)),
    not (is_prime (n+7)),
    practical n,
    practical (n-8),
    practical (n-4),
    practical (n+4),
    practical (n+8)
  ]

main :: IO String
main = return $ show $ sum $ take 4 $ paradises ()

answer :: String
answer = "2039506520"
