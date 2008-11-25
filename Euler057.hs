module Euler057 where
import Ratio
import List (inits)

------------------------------------------------------------------------------
-- 57. Investigate the expansion of the continued fraction for the square root of two.

{-

continued fraction expansions for sqrt(2):
1/1, 3/2, 7/5, 17/12, 41/29
1 + 1/2 = 3/2 = 1.5
1 + 1/(2 + 1/2) = 7/5 = 1.4
1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...

E(0) = 1
E(n+1) = 1 + 1 / (1 + E(n))

E(n) = a/b
E(n+1) = 1 + 1 / (1 + a/b)
E(n+1) = 1 + 1 / ((a+b)/b)
E(n+1) = 1 + b/(a+b)
E(n+1) = (a+b+b)/(a+b)
-}
{-
continued_fraction [] = 0
continued_fraction [n] = n%1
continued_fraction (n:ns) = n%1 + recip (continued_fraction ns)

continued_fraction' [] = (1, 0)
continued_fraction' (n:ns) =
  let (x,y) = continued_fraction' ns in (n*x + y, x)

root_two_seq =
  map continued_fraction $
  drop 2 $ inits $ 1 : repeat 2
-}

root2_seq = f 3 2
  where f a b = (a,b) : f (a+b+b) (a+b)

num_digits = length . show

prob57 m = length $
  filter (\(a,b) -> num_digits a > num_digits b)
  (take m root2_seq)

main :: IO String
main = return $ show $ prob57 1000
-- 153