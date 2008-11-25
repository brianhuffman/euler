module Euler002 where

------------------------------------------------------------------------------
-- 2. Find the sum of all the even-valued terms in the Fibonacci sequence which do not exceed one million.

fibs = zipWith (+) (0 : fibs) (0 : 1 : fibs)

prob2 n = sum $ filter even $ takeWhile (<= n) $ fibs
-- prob2 1000000 = 1089154
-- prob2 4000000 = 4613732

-- more efficient version
{-
fib n is even <--> 3 divides n

F(n+6)
= F(n+5) + F(n+4)
= 2F(n+4) + F(n+3)
= 3F(n+3) + 2F(n+2)
= 3F(n+3) + F(n+2) + F(n+1) + F(n)
= 4F(n+3) + F(n)
-}
evenfibs = f 0 2
  where f a b = a : f b (a + 4*b)

prob2' n = sum . takeWhile (<= n) $ evenfibs

main :: IO String
main = return $ show $ prob2' 4000000