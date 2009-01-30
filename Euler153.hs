module Euler153 where
import SquareRoot
import Data.Array

{-
Problem 153
Investigating Gaussian Integers

05 May 2007

As we all know the equation x^2=-1 has no solutions for real x. If we
however introduce the imaginary number i this equation has two
solutions: x=i and x=-i.  If we go a step further the equation
(x-3)^2=-4 has two complex solutions: x=3+2i and x=3-2i. x=3+2i and
x=3-2i are called each others' complex conjugate.  Numbers of the form
a+bi are called complex numbers. In general a+bi and a-bi are each
other's complex conjugate.

A Gaussian Integer is a complex number a+bi such that both a and b are
integers.  The regular integers are also Gaussian integers (with
b=0). To distinguish them from Gaussian integers with b <> 0 we call
such integers "rational integers."  A Gaussian integer is called a
divisor of a rational integer n if the result is also a Gaussian
integer.

If for example we divide 5 by 1+2i we can simplify 5/(1+2i) in the
following manner:

Multiply numerator and denominator by the complex conjugate of 1+2i:
1-2i.

               5      5   1-2i   5(1-2i)    5(1-2i)   5(1-2i)
The result is ---- = ---- ---- = -------- = ------- = ------- = 1-2i.
              1+2i   1+2i 1-2i   1-(2i)^2   1-(-4)       5

So 1+2i is a divisor of 5.

Note that 1+i is not a divisor of 5 because 5/(1+i) = 5/2 - 5/2 i.

Note also that if the Gaussian Integer (a+bi) is a divisor of a
rational integer n, then its complex conjugate (a-bi) is also a
divisor of n.

In fact, 5 has six divisors such that the real part is positive:
{1, 1+2i, 1-2i, 2+i, 2-i, 5}.

The following is a table of all of the divisors for the first five
positive rational integers:

n       Gaussian integer divisors       Sum s(n) of
        with positive real part         these divisors
1       1                               1
2       1, 1+i, 1-i, 2                  5
3       1, 3                            4
4       1, 1+i, 1-i, 2, 2+2i, 2-2i,4    13
5       1, 1+2i, 1-2i, 2+i, 2-i, 5      12

For divisors with positive real parts, then, we have:
SUM n=1..5. s(n) = 35.

For 1 <= n <= 10^5, SUM s(n) = 17924657155.

What is SUM s(n) for 1 <= n <= 10^8?
-}

{-
Analysis:

n / (a + bi)
n(a-bi) / (a+bi)(a-bi)
n(a-bi) / (a^2 + b^2)
(a^2 + b^2) divides n*a and n*b

When is the product of two Gaussian integers a rational integer?
(a+bi)(c-di) = (ac + bd) + (bc - ad)i = n
* ad = bc
* ac + bd = n

For relatively prime a and b, ad = bc only holds if (c,d) has the
form (ka, kb) for some k.
n = ac + bd = a(ka) + b(kb) = k(a^2 + b^2)

(b-ai)(d+ci) = (ac + bd) + (bc - ad)i = (a+bi)(c+di)
This means that (b-ai) is a factor of n iff (a+bi) is a factor of n.

-}

{-
Sum of all real factors of all integers up to m:
Each value n will appear as a factor (m `div` n) times.

sum [ n * (m `div` n) | n <- [1 .. m] ]
sum [ m - (m `mod` n) | n <- [1 .. m] ]
sum [ m | n <- [1..m] ] - sum [ m `mod` n | n <- [1 .. m] ]
m*m - sum [ m `mod` n | n <- [1 .. m] ]
-}

type Z = Integer

-- sum of integers less than n
triangle' :: Z -> Z
triangle' n = (n * (n - 1)) `div` 2

-- sum [ m `mod` n | n <- [1 .. m] ]
sum_mods :: Z -> Z
sum_mods m = head_mods + sum [ tail_mods nd | nd <- zip [1 ..] ds ]
  where
    divs = [ (n, m`div`n) | n <- [1 .. m] ]
    qs = map snd $ takeWhile (\(n,q) -> n <= q) divs
    q' = last qs
    ds = zipWith subtract (tail qs) qs
    tail_mods (n,d) = d * (m `mod` n) + n * (triangle' d)
    head_mods = sum [ m `mod` n | n <- [1 .. q'] ]

-- SUM n={1..m}. sum_factors n
-- A024916
sum_real_factors_upto :: Z -> Z
sum_real_factors_upto m = m*m - sum_mods m

-- 10^4: 60
-- 10^5: 160
-- 10^6: 480

--------------------------------------------------------------

sums_squares_upto :: Z -> [(Z, Z)]
sums_squares_upto m = (2,2) :
  [ (a^2 + b^2, 2*(a+b)) |
    a <- takeWhile (\a -> a^2 < m) [1 ..],
    b <- takeWhile (\b -> a^2 + b^2 <= m) [a+1 ..],
    gcd a b == 1 ]
-- (2,2) represents factors (1+i) + (1-i)
-- (a^2+b^2, 2*(a+b)) represents factors (a+bi) + (a-bi) + (b+ai) + (b-ai)

-- slightly faster for large values of m
sums_squares_upto' :: Z -> [(Z, Z)]
sums_squares_upto' m = (2,2) : p [(0,1,1,1)]
  where
    p [] = []
    p ((a,b,c,d) : rest)
      | e^2 + f^2 > m = p rest
      | otherwise = (e^2 + f^2, 2*(e+f)) : p ((a,b,e,f):(e,f,c,d):rest)
      where (e,f) = (a+c, b+d)

sum_complex_factors_upto :: Z -> Z
sum_complex_factors_upto m = sum
  [ s * srf (m`div`n) |
    (n, s) <- sums_squares_upto' m ]
  where
    msize = square_root m `div` 2
    a = listArray (1, msize) (map sum_real_factors_upto [1 .. msize])
    srf n = if n <= msize then a!n else sum_real_factors_upto n

prob153 :: Z -> Z
prob153 m = sum_real_factors_upto m + sum_complex_factors_upto m

main :: IO String
main = return $ show $ prob153 (10^8)

answer :: String
answer = "17971254122360635"
