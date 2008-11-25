module Euler158 where
import EulerLib

------------------------------------------------------------------------------
-- 158. Exploring strings for which only one character comes lexicographically after its neighbour to the left.
{-
Problem 158

15 June 2007

Taking three different letters from the 26 letters of the alphabet, character
strings of length three can be formed. Examples are 'abc', 'hat' and 'zyx'.

When we study these three examples we see that for 'abc' two characters come
lexicographically after its neighbour to the left. For 'hat' there is exactly
one character that comes lexicographically after its neighbour to the left.
For 'zyx' there are zero characters that come lexicographically after its
neighbour to the left.

In all there are 10400 strings of length 3 for which exactly one character
comes lexicographically after its neighbour to the left.

We now consider strings of n <= 26 different characters from the alphabet.
For every n, p(n) is the number of strings of length n for which exactly one
character comes lexicographically after its neighbour to the left.

What is the maximum value of p(n)?
-}

{-
Given a list S of n elements, partition it into two sets A and B.

Next, produce a string by concatenating the sorted contents of A
followed by the sorted contents of B.

If the last element of A comes after the first element of B, then
this will produce a valid string.

The only way this fails to produce a valid string is if A contains
exactly the first k elements of S, for 0 <= k <= n. (n+1 possibilities)
-}

-- num_perms n = number of permutations of n elements
-- such that one adjacent pair is out of order.
num_perms :: Int -> Integer
num_perms n = 2^n - toInteger (n + 1)

-- num_seqs m n = sequences of length n chosen from m elements,
-- such that one adjacent pair is out of order.
num_seqs :: Int -> Int -> Integer
num_seqs m n = choose m n * num_perms n

prob158 :: Int -> Integer
prob158 m = maximum $ map (num_seqs m) [1 .. m]

main :: IO String
main = return $ show $ prob158 26
-- 409511334375
