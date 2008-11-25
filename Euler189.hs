module Euler189 where
import EulerLib
import List

------------------------------------------------------------------------------
-- 189. Tri-colouring a triangular grid
{-
# of colorings
--------------
size 1: 3
size 2: 24
size 3: 528
size 4: 31968
-}
{-
    /a\
  /c\b/d\
/g\e/h\f/i\
-}

size3 =
  [ [a,b,c,d,e,f,g,h,i] |
    a <- "rgb",
    b <- "rgb", b /= a,
    c <- "rgb", c /= b,
    d <- "rgb", d /= c,
    e <- "rgb", e /= d,
    f <- "rgb", f /= b,
    g <- "rgb", g /= f,
    h <- "rgb", h /= d, h /= g,
    i <- "rgb", i /= g ]

{-
      /a\
    /c\b/d\
  /g\e/h\f/i\
/m\j/n\k/o\l/p\
-}
size4 =
  [ [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p] |
    a <- xs,

    b <- xs, b /= a,
    c <- xs, c /= b,
    d <- xs, d /= b,

    e <- xs, e /= c,
    f <- xs, f /= d,
    g <- xs, g /= e,
    h <- xs, h /= e, h /= f,
    i <- xs, i /= f,

    j <- xs, j /= g,
    k <- xs, k /= h,
    l <- xs, l /= i,
    m <- xs, m /= j,
    n <- xs, n /= j, n /= k,
    o <- xs, o /= k, o /= l,
    p <- xs, p /= l ]
  where xs = "rgb"

table4 =
  map (\xs -> (fst (head xs), genericLength xs)) $
  groupBy (\x y -> fst x == fst y) $
  sort [ (drop 12 xs, xs) | xs <- size4 ]

table4a =
  [ (key, sum [ n | (k,n) <- table4, ok key k ]) |
    key <- sequence (replicate 4 "rgb") ]
  where
    ok xs ys = and (zipWith (/=) xs ys)

prob189a :: [Integer]
prob189a =
  [ n1 * n2 * n3 |
    [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p] <- size4,
    let key1 = [a,c,g,m],
    let key2 = [a,d,i,p],
    let key3 = [m,n,o,p],
    let Just n1 = lookup key1 table4a,
    let Just n2 = lookup key2 table4a,
    let Just n3 = lookup key3 table4a
  ]

main :: IO String
main = return $ show $ sum prob189a
