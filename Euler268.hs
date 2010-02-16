module Euler268 where
import Primes
import Permutation
import EulerLib (triangle)

{---------------------------------------------------------------------
Problem 268
Counting numbers with at least four distinct prime factors less than
100

11 December 2009

It can be verified that there are 23 positive integers less than 1000
that are divisible by at least four distinct primes less than 100.

Find how many positive integers less than 10^(16) are divisible by at
least four distinct primes less than 100.

---------------------------------------------------------------------}

{---------------------------------------------------------------------

There are 25 primes less than 100:
2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97

2^25 = 32M

-- 2 of 3 --
+1x "ab": ["abc","ab"]
+1x "ac": ["abc","ac"]
+1x "bc": ["abc","bc"]
-2x "abc": ["abc"]
Result: ["abc","ab","ac","bc"]

-- 2 of 4 --
+1x "ab": ["abcd","abc","abd","ab"]
+1x "ac": ["abcd","abc","acd","ac"]
+1x "ad": ["abcd","abd","acd","ad"]
+1x "bc": ["abcd","abc","bcd","bc"]
+1x "bd": ["abcd","abd","bcd","bd"]
+1x "cd": ["abcd","acd","bcd","cd"]
-2x "abc": ["abcd","abc"]
-2x "abd": ["abcd","abd"]
-2x "acd": ["abcd","acd"]
-2x "bcd": ["abcd","bcd"]
+3x "abcd": ["abcd"]
Result: ["abcd","abc","abd","ab","acd","ac","ad","bcd","bc","bd","cd"]

-- 2 of 5 --
+1x "ab"
+1x "ac"
+1x "ad"
+1x "ae"
+1x "bc"
+1x "bd"
+1x "be"
+1x "cd"
+1x "ce"
+1x "de"
-2x "abc"
-2x "abd"
-2x "abe"
-2x "acd"
-2x "ace"
-2x "ade"
-2x "bcd"
-2x "bce"
-2x "bde"
-2x "cde"
+3x "abcd"
+3x "abce"
+3x "abde"
+3x "acde"
+3x "bcde"
-4x "abcde"

-- 3 of 4 --
+1x "abc": ["abcd","abc"]
+1x "abd": ["abcd","abd"]
+1x "acd": ["abcd","acd"]
+1x "bcd": ["abcd","bcd"]
-3x "abcd": ["abcd"]

-- 3 of 5 --
+1x "abc"
+1x "abd"
+1x "abe"
+1x "acd"
+1x "ace"
+1x "ade"
+1x "bcd"
+1x "bce"
+1x "bde"
+1x "cde"
-3x "abcd"
-3x "abce"
-3x "abde"
-3x "acde"
-3x "bcde"
+6x "abcde"


1  0  0  0  0  0  0  0
1  1  0  0  0  0  0  0
1  2  1  0  0  0  0  0
1  3  3  1  0  0  0  0
1  4  6  4  1  0  0  0
1  5 10 10  5  1  0  0
1  6 15 20 15  6  1  0
1  7 21 35 35 21  7  1


Desired set: ["abcd","abc","abd","ab","acd","ac","ad","bcd","bc","bd","cd"]

"abcde"
"abcd"
"abce"
"abc"
"abde"
"abd"
"abe"
"ab"
"acde"
"acd"
"ace"
"ac"
"ade"
"ad"
"ae"
"a"
"bcde"
"bcd"
"bce"
"bc"
"bde"
"bd"
"be"
"b"
"cde"
"cd"
"ce"
"c"
"de"
"d"
"e"
""




---------------------------------------------------------------------}

-- brute force version

has4 = (>=4) . length . takeWhile (<100) . map fst . prime_factorization

brute n = length (filter has4 [1..n])

-- inclusion-exclusion version

multipliers = [0,0,0,0,1,-4,10,-20,35,-56,84,-120,165,-220,286,-364]
-- from column 4 of pascal triangle

ps100 = takeWhile (<100) primes

inex4 tmax = go 1 multipliers ps100
  where
    go x ms ps | x > tmax = 0
    go x ms [] = (tmax `div` x) * head ms
    go x ms (p : ps) = go x ms ps + go (x*p) (tail ms) ps

main :: IO String
main = return $ show $ inex4 (10^16)

answer :: String
answer = "785478606870985"
