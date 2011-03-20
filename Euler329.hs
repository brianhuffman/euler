module Euler329 where
import EulerLib (funArray)
import Data.Array
import Data.Ratio
import Primes

{-

Problem 329
Prime Frog
20 March 2011

Susan has a prime frog. Her frog is jumping around over 500 squares
numbered 1 to 500. He can only jump one square to the left or to the
right, with equal probability, and he cannot jump outside the range
[1;500]. (if it lands at either end, it automatically jumps to the
only available square on the next move.)

When he is on a square with a prime number on it, he croaks 'P'
(PRIME) with probability 2/3 or 'N' (NOT PRIME) with probability 1/3
just before jumping to the next square. When he is on a square with a
number on it that is not a prime he croaks 'P' with probability 1/3 or
'N' with probability 2/3 just before jumping to the next square.

Given that the frog's starting position is random with the same
probability for every square, and given that she listens to his first
15 croaks, what is the probability that she hears the sequence
PPPPNNPPPNPPNPN? Give your answer as a fraction p/q in reduced form.

-}

probabilities :: String -> Array Int Rational
probabilities cs = f cs
  where
    ps = funArray (1,500) primeInt
    a0 = accumArray (*) 1 (1,500) []
    next c a = funArray (1,500) g
      where
        g 1 = (if c then 1/3 else 2/3) * (a!2)
        g 500 = (if c then 1/3 else 2/3) * (a!499)
        g n
          | (ps!n) == c = 2/3 * (a!(n-1)/2 + a!(n+1)/2)
          | otherwise   = 1/3 * (a!(n-1)/2 + a!(n+1)/2)
    f [] = a0
    f (c : cs) = next (c == 'P') (f cs)
    
prob329 cs = sum (elems (probabilities cs)) / 500

showFraction x = show (numerator x) ++ "/" ++ show (denominator x)

main :: IO String
main = return $ showFraction $ prob329 "PPPPNNPPPNPPNPN"

answer :: String
answer = "199740353/29386561536000"
