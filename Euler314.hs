module Euler314 where
import Data.List (maximumBy)
import EulerLib
import Data.Array

{-

Problem 314
12 December 2010

The moon has been opened up, and land can be obtained for free, but
there is a catch. You have to build a wall around the land that you
stake out, and building a wall on the moon is expensive. Every country
has been allotted a 500 m by 500 m square area, but they will possess
only that area which they wall in. 251001 posts have been placed in a
rectangular grid with 1 meter spacing. The wall must be a closed
series of straight lines, each line running from post to post.

The bigger countries of course have built a 2000 m wall enclosing the
entire 250 000 m^2 area. The Duchy of Grand Fenwick, has a tighter
budget, and has asked you (their Royal Programmer) to compute what
shape would get best maximum enclosed-area/wall-length ratio.

You have done some preliminary calculations on a sheet of paper. For a
2000 meter wall enclosing the 250 000 m^2 area the
enclosed-area/wall-length ratio is 125. Although not allowed, but to
get an idea if this is anything better: if you place a circle inside
the square area touching the four sides the area will be equal to
π*2502 m^2 and the perimeter will be π*500 m, so the
enclosed-area/wall-length ratio will also be 125.

However, if you cut off from the square four triangles with sides 75
m, 75 m and 75√2 m the total area becomes 238750 m^2 and the perimeter
becomes 1400+300√2 m. So this gives an enclosed-area/wall-length ratio
of 130.87, which is significantly better.

Find the maximum enclosed-area/wall-length ratio. Give your answer
rounded to 8 places behind the decimal point in the form abc.defghijk.

-}


{-
Dynamic programming approach:

Considering all paths from (0,0) to (x,x), various combinations of
area and length are possible. Depending on the area and length of the
rest of the path, it is not immediately clear which sub-path will
optimize the complete path. Some sub-paths can be ruled out
immediately, however: only those sub-paths whose area:length
combinations fall on the convex hull will be retained.

-}

type Z = Int
type R = Double

{-
extend :: (Z, Z) -> [(Z, Z)] -> [(Z, Z)]
extend (x, y) ((a, b) : p) | x*b == y*a = (x+a, y+b) : p
extend xy p = xy : p

flop :: [(Z, Z)] -> [(Z, Z)]
flop p = map (\(x,y) -> (y,x)) (reverse p)

-- length, area, path
table314 :: Z -> Array (Z, Z) [(R, R, [(Z, Z)])]
table314 m = a
  where
    a = funArray ((0, 0), (m, m)) f
    f (i, 0) = [(fromIntegral i, 0, [(i, 0)])]
    f (0, j) = [(fromIntegral j, 0, [(0, j)])]
    f (i, j) | i > j = [ (l, a, flop p) | (l, a, p) <- a ! (j, i) ]
    f (i, j) = hull
               [ (l', a', p') |
                 x <- [0..i],
                 y <- [0..j],
                 x + y > 0,
                 (l, a, p) <- a!(i-x, j-y),
                 let l' = l + sqrt (fromIntegral x^2 + fromIntegral y^2),
                 let a' = a + fromIntegral (x*y)/2 + fromIntegral (y*(i-x)),
                 let p' = extend (x, y) p
               ]
-}

hull ps =
  if p0 == p1 then [p0]
  else [p0] ++ hull_between p0 p1 ps ++ [p1]
  where
    p0 = minimumOf (\(l,a,p) -> l) ps
    p1 = maximumOf (\(l,a,p) -> a) ps

hull_between p0 p1 ps =
  if null ts then []
  else let p' = snd (maximum ts)
       in hull_between p0 p' ps' ++ [p'] ++ hull_between p' p1 ps'
  where
    ts = [ (t, p) | p <- ps, let t = tri p0 p1 p, t > 0 ]
    ps' = map snd ts
    cross (x1,y1) (x2,y2) = x1*y2 - y1*x2
    tri (x1,y1,_) (x2,y2,_) (x3,y3,_) =
      cross (x2-x1, y2-y1) (x3-x1, y3-y1)


{-

(26.49065324324231,[(0,25),(1,5),(1,3),(2,4),(3,4),(2,2),(4,3),(4,2),(3,1),(5,1),(25,0)])

(39.74593699320027,[(0,38),(1,6),(1,4),(2,5),(1,2),(2,3),(3,4),(3,3),(4,3),(3,2),(2,1),(5,2),(4,1),(6,1),(38,0)])

-}

-- prob314 m = maximum [ (a/l, p) | (l, a, p) <- table314 m ! (m, m) ]


--------------------------------------------------------------

can_extend (x, y) [] = True
can_extend (x, y) ((a, b) : p) = x*b < a*y

-- length, area, path
table314 :: Z -> Array Z [(R, R, [(Z, Z)])]
table314 m = a
  where
    a = funArray (0, m) f
    f 0 = [(0, 0, [])]
    f 1 = [(2, 1, [(0, 1)]), (sqrt 2, 1/2, [(1, -1)])]
    f n = hull
          [ (l', a', p') |
            x <- [0..n`div`2],
            y <- [x..n-x],
            x + y > 0,
            (l, a, p) <- a!(n-x-y),
            let l' = l + 2 * sqrt (fromIntegral x^2 + fromIntegral y^2),
            let a' = a + fromIntegral (n^2 - (n-y)^2 - x*y),
            can_extend (x, y) p,
            let p' = (x, y) : p
          ]

prob314 m = maximum [ (a/l, p) | (l, a, p) <- table314 m ! m ]

{-

(132.52756425686317,[(0,122),(1,11),(1,7),(1,5),(1,4),(2,7),(2,6),(2,5),(3,7),(3,6),(4,7),(4,6),(6,8),(6,7),(3,3)])

-}

main :: IO String
main = return $ showFloat 8 $ fst $ prob314 250

answer :: String
answer = "132.52756426"
