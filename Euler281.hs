module Euler281 where
import Primes
import Data.List

{---------------------------------------------------------------------

Problem 281
Pizza Toppings
05 March 2010

You are given a pizza (perfect circle) that has been cut into m·n
equal pieces and you want to have exactly one topping on each slice.

Let f(m,n) denote the number of ways you can have toppings on the
pizza with m different toppings (m ≥ 2), using each topping on exactly
n slices (n ≥ 1). Reflections are considered distinct, rotations are
not.

Thus, for instance, f(2,1) = 1, f(2,2) = f(3,1) = 2 and f(3,2) = 16.
f(3,2) is shown below:

Find the sum of all f(m,n) such that f(m,n) ≤ 10^15.

---------------------------------------------------------------------}

rotations xs = zipWith (++) (tails xs) (inits xs)

canonical xs = xs == minimum (rotations xs)

permutations' :: [(a, Int)] -> [[a]]
permutations' [] = return []
permutations' ys = do
  (x, xs) <- remove1 ys
  xs' <- permutations' xs
  return (x:xs')

remove1 :: [(a, Int)] -> [(a, [(a, Int)])]
remove1 [] = []
remove1 ((x,m):xs) = (x,xs') : [ (y, (x,m):ys) | (y,ys) <- remove1 xs ]
  where xs' = if m == 1 then xs else (x, m-1) : xs

pizzas :: Int -> Int -> [[Int]]
pizzas m n =
  [ xs |
    xs <- map (1:) (permutations' ((1,n-1) : [ (i,n) | i <- [2..m] ])),
    canonical xs ]

prob281a :: Int -> Int -> Int
prob281a m n = length (pizzas m n)

{---------------------------------------------------------------------

f(m,n) on row n, column m 
n |    2     3     4     5     6     7     8
--------------------------------------------
1 |    1     2     6    24   120   720  5040 . . . . . . . . . .
2 |    2    16   318 11352     .     .     . . .
3 |    4   188 30804     .     .     .
4 |   10  2896     .     .     .
5 |   26 50452     .     .
6 |   80 953056    .
7 |  246     .     .
8 |  810     .
9 | 2704     .
10| 9252

----------------------------------------------------------------------

Define g(m,n) as the number of ways to have n each of m different
toppings, where rotations ARE considered distinct.

Then g(m,n) = (m⋅n)! / (n!)^m
g(m,1) = m!
g(m,2) = (2m)!/2^m
g(m,3) = (3m)!/6^m
g(m,4) = (4m)!/24^m
...

g(m,1) = m⋅f(m,1)
g(m,2) = 2m⋅(f(m,2)-f(m,1)) + g(m,1)
g(m,3) = 3m⋅(f(m,3)-f(m,1)) + g(m,1)
g(m,4) = 4m⋅(f(m,4)-f(m,2)) + g(m,2)
g(m,5) = 5m⋅(f(m,5)-f(m,1)) + g(m,1)
g(m,6) = 6m⋅(f(m,6)-f(m,3)-f(m,2)+f(m,1)) + g(m,3) + g(m,2) - g(m,1)
g(m,7) = 7m⋅(f(m,7)-f(m,1)) + g(m,1)

Solving for f(m,n):

f(m,1) = (m-1)!
f(m,2) = (g(m,2) - g(m,1))/2m + f(m,1)
f(m,3) = (g(m,3) - g(m,1))/3m + f(m,1)
f(m,4) = (g(m,4) - g(m,2))/4m + f(m,2)
f(m,5) = (g(m,5) - g(m,1))/5m + f(m,1)
f(m,6) = (g(m,6) - g(m,3) - g(m,2) + g(m,1))/6m + f(m,3) + f(m,2) - f(m,1)
f(m,7) = (g(m,7) - g(m,1))/7m + f(m,1)
f(m,8) = (g(m,8) - g(m,4))/8m + f(m,4)
f(m,9) = (g(m,9) - g(m,3))/9m + f(m,3)

---------------------------------------------------------------------}

fact n = product [2..n]

g m n = fact (m*n) `div` (fact n)^m

f1 m = fact (m-1)
f2 m = (g m 2 - g m 1) `div` (2*m) + f1 m
f3 m = (g m 3 - g m 1) `div` (3*m) + f1 m
f4 m = (g m 4 - g m 2) `div` (4*m) + f2 m
f5 m = (g m 5 - g m 1) `div` (5*m) + f1 m
f6 m = (g m 6 - g m 3 - g m 2 + g m 1) `div` (6*m) + f3 m + f2 m - f1 m
f7 m = (g m 7 - g m 1) `div` (7*m) + f1 m
f8 m = (g m 8 - g m 4) `div` (8*m) + f4 m
f9 m = (g m 9 - g m 3) `div` (9*m) + f3 m

even_odd_products [] = ([1], [])
even_odd_products (p : ps) = (xs', ys')
  where
    (xs, ys) = even_odd_products ps
    xs' = map (p*) ys ++ xs
    ys' = map (p*) xs ++ ys

prob281f :: Integer -> Integer -> Integer
prob281f m n = (gx - gy) `div` (m*n) - (fx - fy)
  where
    f = prob281f
    ps = map fst (prime_factorization n)
    (xs, ys) = even_odd_products ps
    gx = sum [ g m (n`div`x) | x <- xs ]
    gy = sum [ g m (n`div`y) | y <- ys ]
    fx = sum [ f m (n`div`x) | x <- xs, x > 1 ]
    fy = sum [ f m (n`div`y) | y <- ys ]


prob281 :: Integer -> Integer
prob281 fmax = sum (map sum rows)
  where
    row n = takeWhile (<=fmax) [ prob281f m n | m <- [2..] ]
    rows = takeWhile (not . null) [ row n | n <- [1..] ]

main :: IO String
main = return $ show $ prob281 (10^15)

answer :: String
answer = "1485776387445623"
