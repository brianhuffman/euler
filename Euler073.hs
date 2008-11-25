module Euler073 where

------------------------------------------------------------------------------
-- 73. How many fractions lie between 1/3 and 1/2 in a sorted set of reduced proper fractions?
{-
Consider the fraction, n/d, where n and d are positive integers. If n < d and
HCF(n,d)=1, it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d 8 in ascending order of
size, we get:

1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7,
3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that there are 3 fractions between 1/3 and 1/2.

How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper
fractions for d <= 10,000?
-}

type Fraction = (Int, Int)

-- Solution uses Stern-Brocot tree.

mediant :: Fraction -> Fraction -> Fraction
mediant (a, b) (c, d) = (a + c, b + d)

num_between :: Int -> Int
num_between dmax = f (1,3) (1,2)
  where
    f a c = let b = mediant a c
            in if snd b > dmax then 0 else 1 + f a b + f b c

main :: IO String
main = return $ show $ num_between (10^4)
-- 5066251
