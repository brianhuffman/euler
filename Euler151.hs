module Euler151 where
import EulerLib
import Array

------------------------------------------------------------------------------
-- 151. Paper sheets of standard sizes: an expected-value problem.

prob151a :: Array (Int,Int,Int,Int) Double
prob151a = a
  where
    a = funArray ((0,0,0,0),(1,2,4,8)) f
    mult 0 _ = 0
    mult x y = fromIntegral x * y
    f (0,0,0,0) = 0
    f (0,0,0,1) = 0
    f (a2,a3,a4,a5) = recip (fromIntegral n) * sum
      [mult a2 $ a!(a2-1, a3+1, a4+1, a5+1),
       mult a3 $ a!(a2, a3-1, a4+1, a5+1),
       mult a4 $ a!(a2, a3, a4-1, a5+1),
       mult a5 $ a!(a2, a3, a4, a5-1),
       if n == 1 then 1 else 0]
      where n = a2+a3+a4+a5

main :: IO String
main = return $ show $ prob151a!(1,1,1,1)
-- 0.464399

