module Euler178 where
import EulerLib
import Data.Array

------------------------------------------------------------------------------
-- 178. Step Numbers
{-
Consider the number 45656. It can be seen that each pair of consecutive digits
of 45656 has a difference of one. A number for which every pair of consecutive
digits has a difference of one is called a step number.
A pandigital number contains every decimal digit from 0 to 9 at least once.
How many pandigital step numbers less than 10^40 are there?
-}

{-
prob178a ! (x,p,q,n) =
how many digit strings are there with
  - each consecutive pair differs by 1
  - first digit equal to x
  - at least (p) zeros
  - at least (q) nines
  - length exactly n
-}

prob178a = a
  where
    a = funArray ((0,0,0,1),(9,1,1,40)) f
    f (0,p,q,1) = if q == 0 then 1 else 0
    f (9,p,q,1) = if p == 0 then 1 else 0
    f (_,p,q,1) = if p == 0 && q == 0 then 1 else 0
    f (0,p,q,n) = a!(1,0,q,n-1)
    f (9,p,q,n) = a!(8,p,0,n-1)
    f (x,p,q,n) = a!(x+1,p,q,n-1) + a!(x-1,p,q,n-1)

prob178 m = sum $
  [prob178a ! (d,1,1,n) | d <- [1 .. 9], n <- [1 .. m]]
-- 126461847755

main :: IO String
main = return $ show $ prob178 40
