module Euler217 where
import Memoize

{-
Problem 217
14 November 2008

A positive integer with k (decimal) digits is called balanced if its first
ceiling(k/2) digits sum to the same value as its last ceiling(k/2) digits,
where ceiling(x), pronounced ceiling of x, is the smallest integer >= x,
thus ceiling(π) = 4 and ceiling(5) = 5.

So, for example, all palindromes are balanced, as is 13722.

Let T(n) be the sum of all balanced numbers less than 10^n.
Thus: T(1) = 45, T(2) = 540 and T(5) = 334795890.

Find T(47) mod 3^15
-}

{-

-}

type Set = (Integer, Integer)
-- (num, sum)

add :: Set -> Set -> Set
add (nx, sx) (ny, sy) = (nx * ny, nx * sy + ny * sx)

scale :: Integer -> Set -> Set
scale x (n, s) = (n, x*s)

-- how many strings of length l sum to n?
foo_memo :: (Int, Int) -> Integer
foo_memo = f'
  where
    f' = memoize ((0,0), (23,207)) f
    f (0, 0) = 1
    f (0, n) = 0
    f (l, n) = sum [ f' (l-1, n-d) | d <- [0 .. min n 9] ]

-- sum of strings of length l that sum to n
bar_memo :: (Int, Int) -> Integer
bar_memo = f'
  where
    f' = memoize ((0,0), (23,207)) f
    f (0, n) = 0
    f (l, n) = sum
       [ 10 * f' (l-1, n-d) + toInteger d * foo_memo (l-1, n-d) |
         d <- [0 .. min n 9] ]

set_memo :: (Int, Int) -> Set
set_memo x = (foo_memo x, bar_memo x)

{-
for sum-of-23-digits t, there are
f(23,t) possible arrangements for rhs
f(23,t) - f(22,t) possible arrangements for lhs

23 always divides f(23,t), unless 23 divides t;
in that case, f(23,t) == 1 (mod 23).

-}

prob217_odd len = sum
  [ snd (x1 `add` x2 `add` x3 `add` x4) |
    sum23 <- [1 .. 9*len],
    first <- [1 .. min 9 sum23],
    let sum22 = sum23 - first,
    let x1 = scale (10^(2*len)) (1, toInteger first),
    let x2 = scale (10^(len+1)) (set_memo (len-1, sum22)),
    let x3 = scale (10^len) (10, 45),
    let x4 = set_memo (len, sum23)
  ]

prob217_even len = sum
  [ snd (x1 `add` x2 `add` x3) |
    sum23 <- [1 .. 9*len],
    first <- [1 .. min 9 sum23],
    let sum22 = sum23 - first,
    let x1 = scale (10^(2*len-1)) (1, toInteger first),
    let x2 = scale (10^len) (set_memo (len-1, sum22)),
    let x3 = set_memo (len, sum23)
  ]

t1 = 45
t2 = t1 + prob217_even 1
t3 = t2 + prob217_odd 1
t4 = t3 + prob217_even 2
t5 = t4 + prob217_odd 2

t47 = 45 + sum [ prob217_even l + prob217_odd l | l <- [1 .. 23] ]

main :: IO String
main = return $ show $ t47 `mod` (3^15)
-- 6273134
