module Euler074 where
import EulerLib
import Array

------------------------------------------------------------------------------
-- 74. Determine the number of factorial chains that contain exactly sixty non-repeating terms.
{-
The number 145 is well known for the property that the sum of the factorial of
its digits is equal to 145:

1! + 4! + 5! = 1 + 24 + 120 = 145

Perhaps less well known is 169, in that it produces the longest chain of
numbers that link back to 169; it turns out that there are only three such
loops that exist:

169 -> 363601 -> 1454 -> 169
871 -> 45361 -> 871
872 -> 45362 -> 872

It is not difficult to prove that EVERY starting number will eventually get
stuck in a loop. For example,

69 -> 363600 -> 1454 -> 169 -> 363601 (-> 1454)
78 -> 45360 -> 871 -> 45361 (-> 871)
540 -> 145 (-> 145)

Starting with 69 produces a chain of five non-repeating terms, but the longest
non-repeating chain with a starting number below one million is sixty terms.

How many chains, with a starting number below one million, contain exactly
sixty non-repeating terms?
-}

sum_fact_digits :: Int -> Int
sum_fact_digits = sum . map (a!) . show
  where a = listArray ('0','9') (map factorial [0 .. 9])

maxn = sum_fact_digits 1999999

-- precondition: m > 872
chain_length :: Int -> Int -> Int
chain_length m = chain'
  where
    table = funArray (1, m) chain // loops
    chain n = chain' (sum_fact_digits n) + 1
    chain' n = if n <= m then table ! n else chain n
    loops =
      [ (n, 3) | n <- [169,363601,1454], n < m ] ++
      [ (n, 2) | n <- [871,872,45361,45362], n < m ] ++
      [ (n, 1) | n <- [1,2,145,40585], n < m ]

prob74 l m s = length $ filter (==l) $ map (chain_length s) [1 .. m-1]

main :: IO String
main = return $ show $ prob74 60 (10^6) (100000)
-- 402

-- Optimal memo table size determined by experiment
