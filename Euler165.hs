module Euler165 where
import Ratio
import List
import qualified SortedList as S
import Int

------------------------------------------------------------------------------
-- 165. Intersections

type I = Int64
type R = Ratio I
type Pt = (R,R)
type Seg = ((I,I),(I,I))

--------------------------------------------------
-- segments

segments :: Int -> [Seg]
segments n = take n $ tuple (drop 1 (random 290797))
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
-- comparing segments

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
seg_cross :: Seg -> Seg -> (I, I)
seg_cross ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) = (c3, c4)
  where
    c3 = (x2-x1) * (y3-y1) - (y2-y1) * (x3-x1)
    c4 = (x2-x1) * (y4-y1) - (y2-y1) * (x4-x1)

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

partition_seg

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
main = return $ show $ prob165 5000
-- 2868868


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
