module Euler014 where
import Data.Int (Int64)
import Memoize

{-
Problem 14
Longest Collatz Sequence

05 April 2002

The following iterative sequence is defined for the set of positive
integers:

n -> n/2 (n is even)
n -> 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following
sequence: 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1

It can be seen that this sequence (starting at 13 and finishing at 1)
contains 10 terms. Although it has not been proved yet (Collatz
Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one
million.
-}

type Z = Int64

collatz_step :: Z -> Z
collatz_step n
  | n > 10^11 = error "overflow!"
  | even n = n `div` 2
  | otherwise = 3 * n + 1

collatz_seq :: Z -> [Z]
collatz_seq n = n : if n == 1 then [] else collatz_seq (collatz_step n)

collatz_memo :: Z -> Z -> Int
collatz_memo m = len'
  where
    len' = memoizeU (1, m) len
    len n
      | n == 1 = 1
      | even n = 1 + len' (n `div` 2)
      | otherwise = 2 + len' (n + n `div` 2 + 1)

prob14 :: Z -> Z
prob14 m = seq f $ snd $ maximum [ (f n, n) | n <- [1 .. m] ]
  where f = collatz_memo m

main :: IO String
main = return $ show $ prob14 (10^6)

answer :: String
answer = "837799"

{-
Transition A: n -> n/2 (n is even)
Transition B: n -> 3n + 1 (n is odd)

Possible transitions, mod 2:
0 --A--> 0
0 --A--> 1
1 --B--> 0

Possible transitions, mod 3:
0 --A--> 0
1 --A--> 2
2 --A--> 1
0 --B--> 1
1 --B--> 1
2 --B--> 1

If you don't start at a multiple of 3, you will
never get to a multiple of 3.

Tree of transitions in reverse:
1 - 2 - 4 - 8 - 16 - 32 - 64 - 128 - 256 - 512 -
        |        |         |           |
        1        |        21...       85 - 170 -
                 |
                 5 - 10 - 20 - 40 - 80 - 160 -
                      |         |          |
                      3 ...     |         53 - 
                                |
                               13 - 26 - 52 -
                                          |
                                         17 -

(3n+1)/2 = (2n + n+1)/2 = n + (n+1)/2
-}

{-
Multiple-step transitions:
2k+0 -> k
2k+1 --> 3k+2

4k+0 --> k
4k+1 ---> 3k+1
4k+2 --> 3k+2
4k+3 ----> 9k+8

8k+0 ---> k
8k+1 -----> 9k+2
8k+2 ----> 3k+1
8k+3 -----> 9k+4
8k+4 ----> 3k+2
8k+5 ----> 3k+2
8k+6 -----> 9k+8
8k+7 ------> 27k+26

16k+0 ----> k
16k+1 ------> 9k+1
16k+2 ------> 9k+2
16k+3 ------> 9k+2
16k+4 -----> 3k+1
16k+5 -----> 3k+1
16k+6 ------> 9k+4
16k+7 -------> 27k+13
16k+8 -----> 3k+2
16k+9 -------> 27k+17
16k+10 -----> 3k+2
16k+11 -------> 27k+20
16k+12 ------> 9k+8
16k+13 ------> 9k+8
16k+14 -------> 27k+26
16k+15 --------> 81k+80
-}

