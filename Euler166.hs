module Euler166 where

{-
Problem 166
Criss Cross

03 November 2007

A 4x4 grid is filled with digits d, 0 <= d <= 9.

It can be seen that in the grid

6 3 3 0
5 0 4 3
0 7 1 4
1 2 4 5

the sum of each row and each column has the value 12.
Moreover the sum of each diagonal is also 12.

In how many ways can you fill a 4x4 grid with the digits d, 0 <= d <= 9
so that each row, each column, and both diagonals have the same sum? 
-}

{-
Analysis:

Label the grid elements thus:
a b c d
e f g h
i j k l
m n o p

List of constraints:
t = a + b + c + d  (row 1)
t = e + f + g + h  (row 2)
t = i + j + k + l  (row 3)
t = m + n + o + p  (row 4)
t = a + e + i + m  (col 1)
t = b + f + j + n  (col 2)
t = c + g + k + o  (col 3)
t = d + h + l + p  (col 4)
t = a + f + k + p  (diag 1)
t = d + g + j + m  (diag 2)

Derived constraints:
t = e + f + j + k  (center)
t = a + d + m + p  (corners)

1111  ....  ....  ....
....  1111  ....  ....
....  ....  1111  ....
....  ....  ....  1111

1...  .1..  ..1.  ...1
1...  .1..  ..1.  ...1
1...  .1..  ..1.  ...1
1...  .1..  ..1.  ...1

1...  ...1
.1..  ..1.
..1.  .1..
...1  1...

1..1  ....  .11.  ....
....  .11.  ....  1..1
....  .11.  ....  1..1
1..1  ....  .11.  ....


(diag1 + diag2 + col2 + col3 - row1 - row4) / 2
(diag1 + diag2 + col1 + col4 - row2 - row3) / 2

 a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  t
--------------------------------------------------
 1  1  1  1  0  0  0  0  0  0  0  0  0  0  0  0 -1
 0  0  0  0  1  1  1  1  0  0  0  0  0  0  0  0 -1
 0  0  0  0  0  0  0  0  1  1  1  1  0  0  0  0 -1
 0  0  0  0  0  0  0  0  0  0  0  0  1  1  1  1 -1
 1  0  0  0  1  0  0  0  1  0  0  0  1  0  0  0 -1
 0  1  0  0  0  1  0  0  0  1  0  0  0  1  0  0 -1
 0  0  1  0  0  0  1  0  0  0  1  0  0  0  1  0 -1
 0  0  0  1  0  0  0  1  0  0  0  1  0  0  0  1 -1
 1  0  0  0  0  1  0  0  0  0  1  0  0  0  0  1 -1
 0  0  0  1  0  0  1  0  0  1  0  0  1  0  0  0 -1

This matrix has rank 9; thus there are only 9 independent constraints.

17 components - 9 constraints = 8 degrees of freedom.

This means that we only need to have 8 nested loops;
all other variables may be calculated from these.
-}

prob166 :: Int -> Int
prob166 z = sum
  [ 2 |
    a <- [1 .. z`div`2],
    b <- ds,
    c <- ds,
    d <- ds,
    let t = a+b+c+d,
    m <- ds,
    let p = t-a-d-m, 1<=p, p<=z,
    n <- ds,
    let o = t-m-n-p, 1<=o, o<=z,
    f <- ds,
    let j = t-b-f-n, 1<=j, j<=z,
    let k = t-a-f-p, 1<=k, k<=z,
    let g = t-f-j-k, 1<=g, g<=z,
    e <- ds,
    let h = t-e-f-g, 1<=h, h<=z,
    let i = t-a-e-m, 1<=i, i<=z,
    let l = t-d-h-p, 1<=l, l<=z
  ]
  where ds = [1 .. z]
-- TODO: factor out more symmetries

main :: IO String
main = return $ show $ prob166 10

answer :: String
answer = "7130034"
