module Euler066 where
import EulerLib
import ContinuedFraction
import Data.List
import qualified SortedList as S

------------------------------------------------------------------------------
-- 66. Investigate the Diophantine equation x^2 − Dy^2 = 1.
{-
Consider quadratic Diophantine equations of the form:

x^2 – Dy^2 = 1

For example, when D = 13, the minimal solution in x is 649^2 – 13*180^2 = 1.

It can be assumed that there are no solutions in positive integers when D
is square.

By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the
following:

3^2 – 2*2^2 = 1
2^2 – 3*1^2 = 1
9^2 – 5*4^2 = 1
5^2 – 6*2^2 = 1
8^2 – 7*3^2 = 1

Hence, by considering minimal solutions in x for D ≤ 7, the largest x is
obtained when D = 5.

Find the value of D ≤ 1000 in minimal solutions of x for which the largest
value of x is obtained.
-}

{-
x^2 - D*y^2 = 1
x^2 - 1 = D*y^2

Minimal (x, y) will always be convergents of the
continued fraction for sqrt(D).
-}

-- minimal_solution d = (x, y, d) such that x, y are
-- minimal solution to x^2 - d*y^2 == 1
minimal_solution :: Integer -> (Integer, Integer, Integer)
minimal_solution d = f zs
  where
    (x0, xs) = sqrt_cfraction_cycle d
    ys = x0 : cycle xs
    zs = convergents ys
    f ((x,y):xys)
      | x^2 - d*y^2 == 1 = (x, y, d)
      | otherwise = f xys

squares :: [Integer]
squares = map square [1 ..]

nonsquares :: [Integer]
nonsquares = S.deleteFirsts [1 ..] squares

prob66 :: Integer -> (Integer, Integer, Integer)
prob66 m =
  maximum $
  map minimal_solution $
  takeWhile (<= m) nonsquares

main :: IO String
main = return $ show $ thd3 $ prob66 1000
-- 661

