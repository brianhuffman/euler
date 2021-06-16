module Euler165 where
import Ratio
import Data.List
import qualified SortedList as S
import Int

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

type End = (Int, Int)
type Seg = (End, End)

type R = Ratio Int
type Pt = (R, R)

--------------------------------------------------
-- segments

segments :: Int -> [Seg]
segments n = take n $ tuple $ drop 1 $ random 290797
  where
    tuple (a:b:c:d:xs) = ((a,b),(c,d)) : tuple xs
    random a = (a `mod` 500) : random (a * a `mod` 50515093)

seg1, seg2, seg3 :: Seg
seg1 = ((27,44),(12,32))
seg2 = ((46,53),(17,62))
seg3 = ((46,70),(22,40))

test_segs :: [Seg]
test_segs = [seg1, seg2, seg3]

--------------------------------------------------
-- comparing segments to vertical/horizontal lines

seg_compare_x :: Int -> Int -> Int -> Seg -> Ordering
seg_compare_x ymin ymax xcut ((x0,y0),(x1,y1)) =
  case (compare x0 xcut, compare x1 xcut) of
    (LT, GT) -> test (x0,y0) (x1,y1)
    (GT, LT) -> test (x1,y1) (x0,y0)
    (LT, _) -> LT
    (_, LT) -> LT
    (_, _) -> GT
  where
    -- precondition: x0 < xcut < x1
    test (x0,y0) (x1,y1)
        -- segment crosses cut above ymax
      | (y1-y0)*(xcut-x0) >= (ymax-y0)*(x1-x0) = compare y0 y1
        -- segment crosses cut below ymin
      | (y1-y0)*(xcut-x0) <= (ymin-y0)*(x1-x0) = compare y1 y0
      | otherwise = EQ

seg_compare_y :: Int -> Int -> Int -> Seg -> Ordering
seg_compare_y xmin xmax ycut ((x0,y0),(x1,y1)) =
  seg_compare_x xmin xmax ycut ((y0,x0),(y1,x1))

--------------------------------------------------
-- filtering segments by bounding boxes

type Box = Seg -- ((xmin,ymin), (xmax,ymax))

box500 :: Box
box500 = ((0,0),(500,500))

shorter :: Int -> [a] -> Bool
shorter 0 xs = null xs
shorter n [] = True
shorter n (x : xs) = shorter (n-1) xs

filter_segs :: (Box, [Seg]) -> [(Box, [Seg])]
filter_segs (box@((xmin,ymin),(xmax,ymax)), segs)
  | shorter nmax segs = [(box, segs)]
  | dx * dy == 1 = [(box,segs)]
  | dx < dy = let ycut = avg ymin ymax
                  split = seg_compare_y xmin xmax ycut
                  (segs1, segs2) = f split [] [] segs
                  box1 = ((xmin,ymin),(xmax,ycut))
                  box2 = ((xmin,ycut),(xmax,ymax))
              in filter_segs (box1, segs1) ++ filter_segs (box2, segs2)
  | otherwise = let xcut = avg xmin xmax
                    split = seg_compare_x ymin ymax xcut
                    (segs1, segs2) = f split [] [] segs
                    box1 = ((xmin,ymin),(xcut,ymax))
                    box2 = ((xcut,ymin),(xmax,ymax))
                in filter_segs (box1, segs1) ++ filter_segs (box2, segs2)
  where
    nmax = 3
    dx = xmax - xmin
    dy = ymax - ymin
    avg a b = (a + b) `div` 2
    f c xs ys [] = (xs, ys)
    f c xs ys (seg:segs) =
      case c seg of
        LT -> f c (seg:xs) ys segs
        GT -> f c xs (seg:ys) segs
        EQ -> f c (seg:xs) (seg:ys) segs



--------------------------------------------------
-- comparing segments

-- GT means to the left, LT means to the right
pt_compare :: Seg -> End -> Ordering
pt_compare ((x1,y1),(x2,y2)) (x3,y3) =
  compare ((x2-x1) * (y3-y1)) ((y2-y1) * (x3-x1))

data SegOrdering = Above | Below | Across | OnTop

seg_compare :: Seg -> Seg -> SegOrdering
seg_compare ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) =
  case compare c3 0 of
    LT -> if c4 > 0 then Across else Below
    GT -> if c4 < 0 then Across else Above
    EQ -> case compare c4 0 of
      LT -> Below
      EQ -> OnTop
      GT -> Above
  where
    c3 = (x2-x1) * (y3-y1) - (y2-y1) * (x3-x1)
    c4 = (x2-x1) * (y4-y1) - (y2-y1) * (x4-x1)

-- positive means to the left, negative means to the right
{-
seg_cross :: Seg -> Seg -> (R, R)
seg_cross ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) = (c3, c4)
  where
    c3 = (x2-x1) * (y3-y1) - (y2-y1) * (x3-x1)
    c4 = (x2-x1) * (y4-y1) - (y2-y1) * (x4-x1)
-}

seg_crosses :: Seg -> Seg -> Bool
seg_crosses ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) =
  (a < 0 && 0 < b) || (b < 0 && 0 < a)
  where
    a = (x2-x1) * (y3-y1) - (y2-y1) * (x3-x1)
    b = (x2-x1) * (y4-y1) - (y2-y1) * (x4-x1)

seg_intersects :: Seg -> Seg -> Bool
seg_intersects seg1 seg2 =
  seg_crosses seg1 seg2 && seg_crosses seg2 seg1

--------------------------------------------------
-- finding intersections

seg_intersection :: Seg -> Seg -> Pt
seg_intersection seg1 seg2 = (x, y)
  where
    prep ((x1,y1),(x2,y2)) = (x1-x2, y1-y2, x1*y2 - x2*y1)
    (dx1, dy1, cxy1) = prep seg1
    (dx2, dy2, cxy2) = prep seg2
    d = (dx2 * dy1 - dx1 * dy2)
    x = (dx1 * cxy2 - dx2 * cxy1) % d
    y = (dy1 * cxy2 - dy2 * cxy1) % d

all_pairs :: [a] -> [(a,a)]
all_pairs [] = []
all_pairs (x:ys) = [ (x,y) | y <- ys ] ++ all_pairs ys

{-
intersection of segments ((x1,y1),(x2,y2)) and ((x3,y3),(x4,y4))

(x,y) = (x1,y1) + u*(x2-x1, y2-y1)
(x,y) = (x3,y3) + v*(x4-x3, y4-y3)

x1 + u*(x2-x1) = x3 + v*(x4-x3) = x
y1 + u*(y2-y1) = y3 + v*(y4-y3) = y

x1(y4-y3) + u*(x2-x1)(y4-y3) = x3(y4-y3) + v*(x4-x3)(y4-y3)
y1(x4-x3) + u*(y2-y1)(x4-x3) = y3(x4-x3) + v*(x4-x3)(y4-y3)

x1(y4-y3) - y1(x4-x3) + u[(x2-x1)(y4-y3) - (y2-y1)(x4-x3)] =
  x3(y4-y3) - y3(x4-x3)

u*[(x2-x1)(y4-y3) - (y2-y1)(x4-x3)] = (x3-x1)(y4-y3) - (y3-y1)(x4-x3)

u = [(x3-x4)(y1-y3) - (x1-x3)(y3-y4)] /
    [(x3-x4)(y1-y2) - (x1-x2)(y3-y4)]

v = [(x3-x1)(y1-y2) - (x1-x2)(y3-y1)] /
    [(x3-x4)(y1-y2) - (x1-x2)(y3-y4)]
-}

--------------------------------------------------
-- filtering and splitting

{-
filter_segs :: Seg -> [Seg] -> ([Seg], [Seg], [Seg], [Pt])
filter_segs key segs = f segs [] [] [] []
  where
    f [] lt gt eq pts = (lt, gt, eq, pts)
    f (seg@(a,b):segs) lt gt eq pts =
      case (pt_compare key a, pt_compare key b) of
        (LT, GT) -> f segs ((a,c):lt) ((c,b):gt) eq (c:pts)
        (LT, _ ) -> f segs (seg:lt) gt eq pts
        (GT, LT) -> f segs ((c,b):lt) ((a,c):gt) eq (c:pts)
        (GT, _ ) -> f segs lt (seg:gt) eq pts
        (EQ, LT) -> f segs (seg:lt) gt eq pts
        (EQ, GT) -> f segs lt (seg:gt) eq pts
        (EQ, EQ) -> f segs lt gt (seg:eq) pts
      where c = seg_intersection key (a,b)

unique_intersections :: [Seg] -> [Pt]
unique_intersections [] = []
unique_intersections [seg] = []
unique_intersections segs = pts' ++ lt_pts ++ gt_pts
  where
    key = head segs
    (lt, gt, eq, pts) = filter_segs key segs
    on c (a,b) = compare a c == compare c b
    pts' = [ c | (n,c) <- multiplicities (sort pts), n > 1 || any (on c) eq ]
    lt_pts = unique_intersections lt
    gt_pts = unique_intersections gt

count_intersections :: [Seg] -> Int
count_intersections [] = 0
count_intersections [seg] = 0
count_intersections segs = length pts' + lt_pts + gt_pts
  where
    key = head segs
    (lt, gt, eq, pts) = filter_segs key segs
    on c (a,b) = compare a c == compare c b
    pts' = [ c | (n,c) <- multiplicities (sort pts), n > 1 || any (on c) eq ]
    lt_pts = count_intersections lt
    gt_pts = count_intersections gt

multiplicities :: [Pt] -> [(Int,Pt)]
multiplicities [] = []
multiplicities (x:xs) =
  case multiplicities xs of
    [] -> [(1,x)]
    ((n,y):ys) -> if x == y then (n+1,y):ys else (1,x):(n,y):ys
-}

--------------------------------------------------
-- removing duplicates

compare_pt :: Pt -> Pt -> Ordering
compare_pt (a,b) (c,d) =
  compare
    (numerator a, denominator a, numerator b, denominator b)
    (numerator c, denominator c, numerator d, denominator d)

repeated :: [(Int,Pt)]
repeated =
  [ (2,(85826%269,64274%269)) -- 915
  , (2,(1571%5,1683%5)) -- 943
  --, (2,(237%1,121%1))
  --, (2,(1317%4,1509%4))
  --, (2,(128215%541,163141%541))
  --, (2,(349%2,659%2))
  --, (2,(36567%328,89419%328))
  --, (2,(160%1,369%1))
  --, (2,(4406%19,3596%19))
  ]

remove_all_duplicates :: [Pt] -> [Pt]
remove_all_duplicates = f . sortBy compare_pt
  where
    f (x:y:zs) | x == y = let (zs1,zs2) = span (x ==) zs
                              l = length zs1 + 2
                          in {-if (l,x) `notElem` repeated
                             then error (show (l,x)) else-}
                             f zs2
                          -- f (dropWhile (x ==) zs)
               | otherwise = x : f (y:zs)
    f (x:xs) = x : f xs
    f [] = []

num_distinct :: [Pt] -> Int
num_distinct [] = 0
num_distinct (x:xs) = 1 + num_distinct ls + num_distinct rs
  where
    (ls,rs) = split xs
    split [] = ([], [])
    split (y:ys) =
      let (ls,rs) = split ys in
      case compare_pt x y of
        LT -> (y:ls, rs)
        EQ -> (ls, rs)
        GT -> (ls, y:rs)

--------------------------------------------------
-- putting it all together

-- partition_seg

intersection_list :: [Seg] -> [[Pt]]
intersection_list [] = []
intersection_list (seg:segs) =
  points : intersection_list segs
  where
    points = [ seg_intersection seg seg' |
               seg' <- segs,
               seg_intersects seg seg' ]

prob165' p n =
  -- length $
  foldl1 S.union $
  map (S.nub . sort . filter p) $
  intersection_list (segments n)

prob165'' p n =
  --sum $ map length $
  filter (not . null . snd) $
  zip [1 ..] $
  map (remove_all_duplicates . filter p) $
  intersection_list (segments n)

-- (   -100) 187131
-- (100-200) 706963
-- (200-250) 496265
-- (250-300) 514258
-- (300-400) 776775
-- (400-   ) 187476
-- ( total ) 2868868

{-
3 extra: 450 <= a < 455
1 extra: 455 <= a < 456, 53 <= b < 54
2 extra: 456 <= a < 457
3 extra: 457 <= a < 458
2 extra: 458 <= a < 459
2 extra: 459 <= a < 460

segment 1264: ((453,51),(485,83))
segment 2889: ((460,58),(402,0))
-}
prob165w n =
  num_distinct $
  concat $
  intersection_list $
  segments n

prob165 n =
  sum $
  map length $
  map remove_all_duplicates $
  intersection_list $
  segments n
-- prob165 1000 = 113849 (113853)
-- prob165 2000 = 463866 (463882)
-- prob165 3000 = 1027115 (1027166)
-- prob165 4000 = 1837706 (1837787)
-- prob165 5000 = 2868868 (2868997)

main :: IO String
-- main = return $ show $ count_intersections (segments 5000)
main = return answer

answer :: String
answer = "2868868"


{-
remove_duplicates :: (Ord a) => [a] -> [a]
remove_duplicates = f . sortBy compare_pt
  where
    f (x:y:zs) | x == y = f (dropWhile (x ==) zs)
               | otherwise = x : f (y:zs)
    f (x:xs) = x : f xs
    f [] = []
-}

{-
num_intersections :: [Seg] -> Int
num_intersections [] = 0
num_intersections (seg:segs) =
  num_intersections segs +
  length (remove_duplicates rs)
  where
    rs = [ r |
      seg' <- segs,
      let (a,b) = seg_cross seg seg',
      (a < 0 && 0 < b) || (b < 0 && 0 < a),
      let (c,d) = seg_cross seg' seg,
      (c < 0 && 0 < d) || (d < 0 && 0 < c),
      let r = c % d ]
-}
