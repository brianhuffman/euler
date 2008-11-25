module Euler110 where
import Euler108 (prob108)

------------------------------------------------------------------------------
-- 110. Find an efficient algorithm to analyse the number of solutions of the equation 1/x + 1/y = 1/n.
{-
In the following equation x, y, and n are positive integers.

1/x + 1/y = 1/n

It can be verified that when n = 1260 there are 113 distinct solutions and
this is the least value of n for which the total number of distinct solutions
exceeds one hundred.

What is the least value of n for which the number of distinct solutions exceeds
four million?

NOTE: This problem is a much more difficult version of problem 108 and as it
is well beyond the limitations of a brute force approach it requires a clever
implementation.
-}

main :: IO String
main = return $ show $ fst $ prob108 (4*10^6)
-- 9350130049860600
