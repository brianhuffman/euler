module Euler160 where
import EulerLib

{-
Problem 160
Factorial trailing digits

07 September 2007

For any N, let f(N) be the last five digits before the trailing zeroes
in N!.  For example,

9! = 362880 so f(9)=36288
10! = 3628800 so f(10)=36288
20! = 2432902008176640000 so f(20)=17664

Find f(1,000,000,000,000)
-}

{-
powers of 5 mod 10^5: cycle of period 8
powers of 2 mod 10^5: cycle of period 2500
5^8 `mod` 10^5 = 90625
2^2500 `mod` 10^5 = 09376
productMod (10^5) $ filter (not . divides 5) [1,3..10^5] = 1
productMod (10^5) $ filter (not . divides 5) [1,3..50000] = 1

For odd a and b, (50000+a)*(50000+b) == a*b (mod 100000)
-}

productMod n = foldl (\x y -> (x * y) `mod` n) 1

exponent_in_factorial p n =
  if n == 0 then 0 else
  let m = n `div` p in m + exponent_in_factorial p m

-- product of odd, non-multiples of 5 up to n, mod 10^5
prob160a n = productMod (10^5) $ filter (not . divides 5) [1,3..n `mod` 50000]

-- product of odd numbers up to n, with all 5's divided off, mod 10^5
prob160b n = if n == 0 then 1 else
  productMod (10^5) [prob160a n, prob160b (n `div` 5)]

-- product of all numbers up to n, with all 2's and 5's divided off, mod 10^5
prob160c n = if n == 0 then 1 else
  productMod (10^5) [prob160b n, prob160c (n `div` 2)]

prob160 n = productMod (10^5) [prob160c n, 2 ^ e2']
  where
    e2 = exponent_in_factorial 2 n
    e5 = exponent_in_factorial 5 n
    e2' = (e2 - e5) `mod` 2500

main :: IO String
main = return $ show $ prob160 (10^12)

answer :: String
answer = "16576"
