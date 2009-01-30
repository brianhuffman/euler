module Euler165 where
import Data.Ratio
import Data.List (sort)

{-
Problem 165
Intersections

27 October 2007

A segment is uniquely defined by its two endpoints.

By considering two line segments in plane geometry there are three
possibilities: the segments have zero points, one point, or infinitely
many points in common.

Moreover when two segments have exactly one point in common it might
be the case that that common point is an endpoint of either one of the
segments or of both. If a common point of two segments is not an
endpoint of either of the segments it is an interior point of both
segments.

We will call a common point T of two segments L_(1) and L_(2) a true
intersection point of L_(1) and L_(2) if T is the only common point of
L_(1) and L_(2) and T is an interior point of both segments.

Consider the three segments L_(1), L_(2), and L_(3):

L_(1): (27, 44) to (12, 32)
L_(2): (46, 53) to (17, 62)
L_(3): (46, 70) to (22, 40)

It can be verified that line segments L_(2) and L_(3) have a true
intersection point. We note that as the one of the end points of
L_(3): (22,40) lies on L_(1) this is not considered to be a true point
of intersection. L_(1) and L_(2) have no common point. So among the
three line segments, we find one true intersection point.

Now let us do the same for 5000 line segments. To this end, we
generate 20000 numbers using the so-called "Blum Blum Shub"
pseudo-random number generator.

s_(0) = 290797
s_(n+1) = s_(n)×s_(n) (modulo 50515093)
t_(n) = s_(n) (modulo 500)

To create each line segment, we use four consecutive numbers
t_(n). That is, the first line segment is given by:

(t_(1), t_(2)) to (t_(3), t_(4))

The first four numbers computed according to the above generator
should be: 27, 144, 12 and 232. The first segment would thus be
(27,144) to (12,232).

How many distinct true intersection points are found among the 5000
line segments?

-}

type Z = Integer
type R = Ratio Z
type Pt = (R, R)

--------------------------------------------------
-- Lines

data Line = Line Z Z Z

-- Line a b c = { (x,y) | a*x + b*y = c }

-- a*x1 + b*y1 = a*x2 + b*y2 = c
make_line :: (Z, Z) -> (Z, Z) -> Line
make_line (x1,y1) (x2,y2) = Line a b c
  where
    a = y1 - y2
    b = x2 - x1
    c = x2*y1 - x1*y2

pt_compare :: Line -> Pt -> Ordering
pt_compare (Line a b c) (x, y) =
  compare (a*nx*dy + b*ny*dx) (c*dx*dy)
  where
    nx = numerator x
    ny = numerator y
    dx = denominator x
    dy = denominator y

line_intersection :: Line -> Line -> Pt
line_intersection (Line a1 b1 c1) (Line a2 b2 c2) = (x%d, y%d)
  where
    x = b2*c1 - b1*c2
    y = a1*c2 - a2*c1
    d = a1*b2 - a2*b1

--------------------------------------------------
-- segments

data Seg = Seg Pt Pt Line

line_of_segment :: Seg -> Line
line_of_segment (Seg _ _ line) = line

make_segment :: (Z, Z) -> (Z, Z) -> Seg
make_segment (x1,y1) (x2,y2) = Seg pt1 pt2 line
  where
    pt1 = (fromIntegral x1, fromIntegral y1)
    pt2 = (fromIntegral x2, fromIntegral y2)
    line = make_line (x1,y1) (x2,y2)

--------------------------------------------------
-- test segments

segments :: Int -> [Seg]
segments n = take n $ tuple $ drop 1 $ random 290797
  where
    tuple (a:b:c:d:xs) = make_segment (a,b) (c,d) : tuple xs
    random a = (a `mod` 500) : random (a * a `mod` 50515093)

seg1, seg2, seg3 :: Seg
seg1 = make_segment (27,44) (12,32)
seg2 = make_segment (46,53) (17,62)
seg3 = make_segment (46,70) (22,40)

test_segs :: [Seg]
test_segs = [seg1, seg2, seg3]

--------------------------------------------------
-- filtering and splitting

filter_segs :: Line -> [Seg] -> ([Seg], [Seg], [Seg], [Pt])
filter_segs pivot segs = f segs [] [] [] []
  where
    f [] lt gt eq pts = (lt, gt, eq, pts)
    f (seg@(Seg a b l):segs) lt gt eq pts =
      case (pt_compare pivot a, pt_compare pivot b) of
        (LT, GT) -> f segs ((Seg a c l):lt) ((Seg c b l):gt) eq (c:pts)
        (LT, _ ) -> f segs (seg:lt) gt eq pts
        (GT, LT) -> f segs ((Seg c b l):lt) ((Seg a c l):gt) eq (c:pts)
        (GT, _ ) -> f segs lt (seg:gt) eq pts
        (EQ, LT) -> f segs (seg:lt) gt eq pts
        (EQ, GT) -> f segs lt (seg:gt) eq pts
        (EQ, EQ) -> f segs lt gt (seg:eq) pts
      where c = line_intersection pivot l

unique_intersections :: [Seg] -> [Pt]
unique_intersections [] = []
unique_intersections [seg] = []
unique_intersections segs = pts' ++ lt_pts ++ gt_pts
  where
    pivot = line_of_segment (head segs)
    (lt, gt, eq, pts) = filter_segs pivot segs
    on c (Seg a b _) = compare a c == compare c b
    pts' = [ c | (n,c) <- multiplicities (sort pts), n > 1 || any (on c) eq ]
    lt_pts = unique_intersections lt
    gt_pts = unique_intersections gt

count_intersections :: [Seg] -> Int
count_intersections [] = 0
count_intersections [seg] = 0
count_intersections segs = length pts' + lt_pts + gt_pts
  where
    pivot = line_of_segment (head segs)
    (lt, gt, eq, pts) = filter_segs pivot segs
    on c (Seg a b _) = compare a c == compare c b
    pts' = [ c | (n,c) <- multiplicities (sort pts), n > 1 || any (on c) eq ]
    lt_pts = count_intersections lt
    gt_pts = count_intersections gt

multiplicities :: [Pt] -> [(Int, Pt)]
multiplicities [] = []
multiplicities (x:xs) =
  case multiplicities xs of
    [] -> [(1,x)]
    ((n,y):ys) -> if x == y then (n+1,y):ys else (1,x):(n,y):ys

prob165 :: Int -> Int
prob165 n = count_intersections (segments n)
-- prob165 1000 = 113849 (113853)
-- prob165 2000 = 463866 (463882)
-- prob165 3000 = 1027115 (1027166)
-- prob165 4000 = 1837706 (1837787)
-- prob165 5000 = 2868868 (2868997)

main :: IO String
main = return $ show $ prob165 5000

answer :: String
answer = "2868868"
