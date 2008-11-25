module Euler147 where
import EulerLib
import Array

-- triangle_seq = scanl1 (+) [0..]

------------------------------------------------------------------------------
-- 147. Rectangles in cross-hatched grids
{-
In a 3x2 cross-hatched grid, a total of 37 different rectangles could be
situated within that grid as indicated in the sketch.

+--+--+--+    +--+--+--+    +--+--+--+    +--+--+--+
|##|\/|\/|    |##|##|\/|    |\/|\/|\/|    |\/#\/|\/|
|##|/\|/\|    |##|##|/\|    |/\|/\|/\|    |/###\|/\|
+--+--+--+ 6  +--+--+--+ 2  +##+--+--+ 7  +#####+--+ 2
|\/|\/|\/|    |##|##|\/|    |\/|\/|\/|    |\###/|\/|
|/\|/\|/\|    |##|##|/\|    |/\|/\|/\|    |/\#/\|/\|
+--+--+--+    +--+--+--+    +--+--+--+    +--+--+--+

+--+--+--+    +--+--+--+    +--+--+--+    +--+--+--+
|##|##|\/|    |##|\/|\/|    |\/#\/|\/|    |\/|\/|\/|
|##|##|/\|    |##|/\|/\|    |/##/\|/\|    |/\|/\|/\|
+--+--+--+ 4  +--+--+--+ 3  +##+--+--+ 4  +##+--+--+ 4
|\/|\/|\/|    |##|\/|\/|    |\/|\/|\/|    |\##\/|\/|
|/\|/\|/\|    |##|/\|/\|    |/\|/\|/\|    |/\#/\|/\|
+--+--+--+    +--+--+--+    +--+--+--+    +--+--+--+

+--+--+--+    +--+--+--+    +--+--+--+    +--+--+--+
|##|##|##|    |##|##|##|    |\/|\/#\/|    |\/#\/|\/|
|##|##|##|    |##|##|##|    |/\|/##/\|    |/\##\|/\|
+--+--+--+ 2  +--+--+--+ 1  +--+##+--+ 1  +--+##+--+ 1
|\/|\/|\/|    |##|##|##|    |\/##/|\/|    |\/|\##\/|
|/\|/\|/\|    |##|##|##|    |/\#/\|/\|    |/\|/\#/\|
+--+--+--+    +--+--+--+    +--+--+--+    +--+--+--+

There are 5 grids smaller than 3x2, vertical and horizontal dimensions being
important, i.e. 1x1, 2x1, 3x1, 1x2 and 2x2. If each of them is cross-hatched,
the following number of different rectangles could be situated within those
smaller grids:

1x1: 1
2x1: 4
3x1: 8
1x2: 4
2x2: 18

Adding those to the 37 of the 3x2 grid, a total of 72 different rectangles
could be situated within 3x2 and smaller grids.

How many different rectangles could be situated within 47x43 and smaller grids?
-}

{-
How many upright tiles does a diagonal rectangle cover?
It may be positioned with left corner at lattice point, or tile center.
1x1: 1x2 or 2x1
1x2: 2x2 or 2x2
1x3: 2x3 or 3x2
1x4: 3x3 or 3x3
2x1: 2x2 or 2x2
2x2: 2x2 or 3x3
2x3: 3x3 or 3x3

+--+--+--+--+
|\/|\/|\/|\/|
|/\|/\|/\|/\|
+--+--+--+--+
|\/|\/|\/|\/|
|/\|/\|/\|/\|
+--+--+--+--+
|\/|\/|\/|\/|
|/\|/\|/\|/\|
+--+--+--+--+
|\/|\/|\/|\/|
|/\|/\|/\|/\|
+--+--+--+--+

m*n rectangle has diagonal rectangles:
size 1*1: m(n-1) + (m-1)n         = 2(m*n) - (m+n)
size 1*2: (m-1)(n-1) + (m-1)(n-1) = 2(m*n) - 2(m+n) + 2
size 2*1: (m-1)(n-1) + (m-1)(n-1) = 2(m*n) - 2(m+n) + 2
size 2*2: (m-1)(n-1) + (m-2)(n-2) = 2(m*n) - 3(m+n) + 5
total: 8mn - 8(m+n) + 9

For each i*j diagonal rectangle that fits into a m*n grid,
there is one (i+2)*(j+2) rectangle that fits into a (m+2)*(n+2) grid.
(Expand grid and rectangle by one tile on all sides.)

For each i*j diagonal rectangle that fits into a m*n grid,
there is one (i+2)*j and one i*(j+2) rectangle that both fit into
a (m+1)*(n+1) grid. (Expand grid and rectangle either on bottom and
right, or top and right.)
-}

type I = Integer

diagonal_rectangles :: I -> I -> Array (I, I) I
diagonal_rectangles w h = a
  where
    a = funArray ((0,0),(w,h)) f
    f (0, n) = 0
    f (m, 0) = 0
    f (1, n) = n - 1
    f (m, 1) = m - 1
    f (m, n) = 8*(m*n - m - n) + 9 +
                 2 * a!(m-1,n-1) - a!(m-2,n-2)

{-
How many upright rectangles in an m*n grid?

uprights m n
= SUM i=1..m. SUM i=1..n. i*j
= (SUM i=1..m. i)*(SUM j=1..n. j)
= triangle m * triangle n

How many upright rectangles in m*n and smaller grids?

uprights_upto m' n'
= SUM m=1..m'. SUM n=1..n'. uprights m n
= SUM m=1..m'. SUM n=1..n'. triangle m * triangle n
= (SUM m=1..m'. triangle m) * (SUM n=1..n'. triangle n)
= pyramid m * pyramid n
-}

upright_rectangles :: I -> I -> Array (I, I) I
upright_rectangles w h = a
  where
    a = funArray ((0,0),(w,h)) f
    f (m, n) = triangle m * triangle n

crosshatch_rectangles :: I -> I -> Array (I, I) I
crosshatch_rectangles w h = a
  where
    a = funArray ((0,0),(w,h)) f
    d = diagonal_rectangles w h
    u = upright_rectangles w h
    f mn = d!mn + u!mn

prob147 :: I -> I -> I
prob147 w h = sum $ elems $ crosshatch_rectangles w h

main :: IO String
main = return $ show $ prob147 47 43
-- 846910284
