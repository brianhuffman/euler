module Euler142 where
import qualified Data.Set as Set

------------------------------------------------------------------------------
-- 142. Perfect Square Collection
{-
Find the smallest x + y + z with integers x > y > z > 0 such that x + y,
x - y, x + z, x - z, y + z, y - z are all perfect squares.
-}

{-
let a = x - y
    b = y - z
    c = y + z

x - y = a
y - z = b
y + z = c
x - z = a + b
x + z = a + c
x + y = a + b + c

squares are equivalent to either 0 or 1 mod 4.
- if m, n, and m+n are all square, then m*n == 0  (mod 2).
- a*b == 0  (mod 2)
- a*c == 0  (mod 2)
- (a+b)*c == 0  (mod 2)
- (a+c)*b == 0  (mod 2)
- b*c == 0  (mod 2)
- b and c differ by an even amount.
- b and c must both be even.

i^2 = a
j^2 = b
k^2 = c
l^2 = a + b
m^2 = a + c
n^2 = a + b + c

Quadratic residues mod 4: [0, 1]
  b and c must both be even

Quadratic residues mod 7: [0, 1, 4]
  

i^2 + j^2 = l^2
i^2 + k^2 = m^2
i^2 + j^2 + k^2 = n^2

j and k are even.
if i is even, then l, m, n are also even.
if everything is even, then we can get a smaller solution by dividing by 2.
 .. assume i, l, m, n are odd.

(2i+1)^2 + (2j)^2 = (2l+1)^2
(2i+1)^2 + (2k)^2 = (2m+1)^2
(2i+1)^2 + (2j)^2 + (2k)^2 = (2n+1)^2

4i^2 + 4i + 1 + 4j^2 = 4l^2 + 4l + 1
4i^2 + 4i + 1 + 4k^2 = 4m^2 + 4m + 1
4i^2 + 4i + 1 + 4j^2 + 4k^2 = 4n^2 + 4n + 1

i^2 + i + j^2 = l^2 + l
i^2 + i + k^2 = m^2 + m
i^2 + i + j^2 + k^2 = n^2 + n

i(i+1) + j^2 = l(l+1)
i(i+1) + k^2 = m(m+1)
i(i+1) + j^2 + k^2 = n(n+1)

2i(i+1) + j^2 + k^2 = l(l+1) + m(m+1)
i(i+1) + j^2 + k^2 = n(n+1)

i(i+1) = l(l+1) + m(m+1) - n(n+1)
i(i+1) + n(n+1) = l(l+1) + m(m+1)
-}

prob142a m =
  [ (x,y,z) |
    i <- [1,3 .. m],
    j <- [2,4 .. m],
    k <- [j+2, j+4 .. m],
    let a = i*i,
    let b = j*j,
    let c = k*k,
    isSquare (a + b),
    isSquare (a + c),
    isSquare (a + b + c),
    let z = (c - b) `div` 2,
    let y = z + b,
    let x = y + a ]
  where
    sqSet = Set.fromList [ x*x | x <- [1,3 .. 2*m] ]
    isSquare x = Set.member x sqSet
-- (434657,420968,150568)

prob142 m = map (\(x,y,z) -> x+y+z) $ prob142a m

main :: IO String
main = return $ show $ minimum $ prob142 1000
-- 1006193

-- TODO: clean this up