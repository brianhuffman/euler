module Euler325 where
import EulerLib

{-

Problem 325
19 February 2011

A game is played with two piles of stones and two players. At her
turn, a player removes a number of stones from the larger pile. The
number of stones she removes must be a positive multiple of the number
of stones in the smaller pile.

E.g., let the ordered pair (6,14) describe a configuration with 6
stones in the smaller pile and 14 stones in the larger pile, then the
first player can remove 6 or 12 stones from the larger pile.

The player taking all the stones from a pile wins the game.

A winning configuration is one where the first player can force a win.
For example, (1,5), (2,6) and (3,12) are winning configurations
because the first player can immediately remove all stones in the
second pile.

A losing configuration is one where the second player can force a win,
no matter what the first player does. For example, (2,3) and (3,4) are
losing configurations: any legal move leaves a winning configuration
for the second player.

Define S(N) as the sum of (xi+yi) for all losing configurations
(xi,yi), 0 < xi < yi ≤ N. We can verify that S(10) = 211 and S(10^4) =
230312207313.

Find S(10^16) mod 7^10.

-}


-- list of quotients from gcd algorithm
-- precondition: a < b
quotients 0 b = []
quotients a b = q : quotients r a
  where
    (q, r) = b `divMod` a

unquotients [] = (0, 1)
unquotients (q : qs) = (b, a + q*b)
  where (a, b) = unquotients qs

winning_qs [] = False
winning_qs (1 : qs) = not (winning_qs qs) -- only one move possible
winning_qs (q : qs) = True                -- can leave either qs or 1:qs.

winning a b = winning_qs (quotients a b)

ones a b = length (takeWhile (==1) (quotients a b))

-- winning configurations start with an even-length sequence of 1s.
-- losing configurations start with an odd-length sequence of 1s.

losing_upto n =
  [ (x, y) | x <- [1 .. n], y <- [x+1 .. n], not (winning x y) ]

slow325 n = sum
  [ x + y | x <- [1 .. n], y <- [x+1 .. n], not (winning x y) ]

table_upto n =
  [ replicate x ' ' ++
    [ "0123456789" !! ones x y | y <- [x+1 .. n] ]
  | x <- [1 .. n] ]

{-

*Euler325> mapM_ print $ table_upto 60
-123456789012345678901234567890123456789012345678901234567890
" 00000000000000000000000000000000000000000000000000000000000"
"  1000000000000000000000000000000000000000000000000000000000"
"   120000000000000000000000000000000000000000000000000000000"
"    11200000000000000000000000000000000000000000000000000000"
"     1132000000000000000000000000000000000000000000000000000"
"      111220000000000000000000000000000000000000000000000000"
"       11132200000000000000000000000000000000000000000000000"
"        1111422000000000000000000000000000000000000000000000"
"         111132220000000000000000000000000000000000000000000"
"          11111322200000000000000000000000000000000000000000"
"           1111134222000000000000000000000000000000000000000"
"            111111322220000000000000000000000000000000000000"
"             11111135222200000000000000000000000000000000000"
"              1111111342222000000000000000000000000000000000"
"               111111133222220000000000000000000000000000000"
"                11111111342222200000000000000000000000000000"
"                 1111111133422222000000000000000000000000000"
"                  111111111352222220000000000000000000000000"
"                   11111111133422222200000000000000000000000"
"                    1111111111334222222000000000000000000000"
"                     111111111133622222220000000000000000000"
"                      11111111111334222222200000000000000000"
"                       1111111111133542222222000000000000000"
"                        111111111111334222222220000000000000"
"                         11111111111133342222222200000000000"
"                          1111111111111335422222222000000000"
"                           111111111111133342222222220000000"
"                            11111111111111335422222222200000"
"                             1111111111111133364222222222000"
"                              111111111111111333422222222220"
"                               11111111111111133354222222222"
"                                1111111111111111333442222222"
"                                 111111111111111133354222222"
"                                  11111111111111111333742222"
"                                   1111111111111111133334422"
"                                    111111111111111111333542"
"                                     11111111111111111133336"
"                                      1111111111111111111333"
"                                       111111111111111111133"
"                                        11111111111111111111"
"                                         1111111111111111111"
"                                          111111111111111111"
"                                           11111111111111111"
"                                            1111111111111111"
"                                             111111111111111"
"                                              11111111111111"
"                                               1111111111111"
"                                                111111111111"
"                                                 11111111111"
"                                                  1111111111"
"                                                   111111111"
"                                                    11111111"
"                                                     1111111"
"                                                      111111"
"                                                       11111"
"                                                        1111"
"                                                         111"
"                                                          11"
"                                                           1"
"                                                            "

ones a b = 0, if 2a <= b
ones a b = 2, if 5a <= 3b and b < 2a
ones a b = 4, if 13a <= 8b and 3b < 5a
...
ones a b = 5, if 8a < 5b and 13b <= 21a
ones a b = 3, if 3a < 2b and 5b <= 8a
ones a b = 1, if a < b and 2b <= 3a

winning a b <--> phi < b/a (golden ratio)

phi < b/a
a/b < 1/phi
a/b < phi - 1
a/b + 1 < phi
(a+b)/b < phi

phi < b/a <--> (a+b)/b < b/a

(a+b)/b < b/a
a(a+b)/b < b
a(a+b) < b^2
-}

-- precondition: a < b
winning' a b = a*(a+b) < b*b

{-

Shape of winning states has Fibonacci pattern.

13x8:
+--+--+--+--+--+--+--+--+--+--+--+--+--+
|  |  |  |#####|########|##############|
|  +--+--+#2x2#|########|##############|
|        |#####|###3x3##|##############|
|        +--+--+########|##############|
|           |  |########|######5x5#####|
|           +--+--+--+--+##############|
|                 |  |  |##############|
|                 +--+--+##############|
|                       |##############|
+-----------------------+--+--+--+--+--+
|                       |  |  |  |  |  |
|                       |  +--+--+--+--+
|                       |        |  |  |
|                       |        +--+--+
|                       |           |  |
+-----------------------+-----------+--+

-}

type Z = Integer

-- [(fibonacci, area, sum_x, sum_y)]
fib_results :: [(Z, Z, Z, Z)]
fib_results = rs
  where
    rs = (1, 0, 0, 0) : (1, 0, 0, 0) : zipWith next rs (tail rs)
    next (n1, a1, x1, y1) (n2, a2, x2, y2) = (n3, a3, x3, y3)
      where
        n3 = n1 + n2
        a3 = a1 + a2 + n1^2
        sq = n1 * triangle n1
        x3 = x1 + x2 + n2*(a1 + n1^2) + sq
        y3 = y1 + y2 + n1*a1 + sq

largest_result_below :: Z -> (Z, Z, Z, Z)
largest_result_below m =
  last (takeWhile (\(n, a, x, y) -> n < m) fib_results)

result :: Z -> (Z, Z, Z, Z)
result 1 = (1, 0, 0, 0)
result 2 = (2, 1, 2, 1)
result n = (n, a3, x3, y3)
  where
    (n2, a2, x2, y2) = largest_result_below n
    (n1, a1, x1, y1) = largest_result_below n2
    (n', a', x', y') = result (n - n2)
    a3 = a' + a2 + n'*n1
    x3 = x' + x2 + n2*(a' + n'*n1) + n1 * triangle n'
    y3 = y' + y2 + n1*a' + n' * triangle n1

sum_all :: Integer -> Integer
sum_all n = (n^3 - n) `div` 2
-- sum_all n = sum [ (x+y) | x <- [1..n], y <- [x+1..n] ]

process (n, a, x, y) = sum_all n - x - y

prob325 :: Integer -> Integer
prob325 n = process (result n)

main :: IO String
main = return $ show (prob325 (10^16) `mod` 7^10)

answer :: String
answer = "54672965"
