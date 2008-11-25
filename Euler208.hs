module Euler208 where
import Memoize
import EulerLib
import Data.Array

{-
A robot moves in a series of one-fifth circular arcs (72°), with a free
choice of a clockwise or an anticlockwise arc for each step, but no turning
on the spot.

One of 70932 possible closed paths of 25 arcs starting northward is

LRLLLRRRRLRRLRLLLLRLLRLLL
5554334511122221544433321
EAEDCDEABABCBCBAEDEDCDCBA

Given that the robot starts facing North, how many journeys of 70 arcs in
length can it take that return it, after the final arc, to its starting
position? (Any arc may be traversed multiple times.)
-}

{-
Directions of movement: 1,2,3,4,5
Possible orientations: A,B,C,D,E  (A = North)

Possible state transitions:
  A <---1---> B
  B <---2---> C
  C <---3---> D
  D <---4---> E
  E <---5---> A
Any transition may be done in either direction,
corresponding to either a left or right turn.

Each of the 5 possible directions of movement (1,2,3,4,5)
must be taken an equal number of times.

For a trip of length 70, each direction must be taken 14 times.

Equivalently, each of the 5 possible orientations (A,B,C,D,E)
must be visited an equal number of times.

State space = 5 * 14^5 = 2689120 entries

-}

data Dir = A | B | C | D | E
  deriving (Eq, Show, Ord, Enum, Ix)

-- (r,(a,b,c,d,e))
-- r = current orientation
-- a,b,c,d,e = how many previous moves in directions 1,2,3,4,5
path_count :: Int -> Array (Int,(Int,Int,Int,Int,Int)) Integer
path_count l = p
  where
    p = funArray ((0,(0,0,0,0,0)), (4,(l,l,l,l,l))) f
    f (0,(0,0,0,0,0)) = 1
    f (0,(a,b,c,d,e)) = (if e>0 then p!(4,(a,b,c,d,e-1)) else 0) +
                        (if a>0 then p!(1,(a-1,b,c,d,e)) else 0)
    f (1,(a,b,c,d,e)) = (if a>0 then p!(0,(a-1,b,c,d,e)) else 0) +
                        (if b>0 then p!(2,(a,b-1,c,d,e)) else 0)
    f (2,(a,b,c,d,e)) = (if b>0 then p!(1,(a,b-1,c,d,e)) else 0) +
                        (if c>0 then p!(3,(a,b,c-1,d,e)) else 0)
    f (3,(a,b,c,d,e)) = (if c>0 then p!(2,(a,b,c-1,d,e)) else 0) +
                        (if d>0 then p!(4,(a,b,c,d-1,e)) else 0)
    f (4,(a,b,c,d,e)) = (if d>0 then p!(3,(a,b,c,d-1,e)) else 0) +
                        (if e>0 then p!(0,(a,b,c,d,e-1)) else 0)

-- ((a,b,c,d,e),r)
-- r = current orientation
-- a,b,c,d,e = how many previous moves in directions 1,2,3,4,5
path_count' :: Int -> Array ((Int,Int,Int,Int,Int),Int) Integer
path_count' l = p
  where
    p = funArray (((0,0,0,0,0),0), ((l,l,l,l,l),4)) f
    f ((0,0,0,0,0),0) = 1
    f ((a,b,c,d,e),0) = (if e>0 then p!((a,b,c,d,e-1),4) else 0) +
                        (if a>0 then p!((a-1,b,c,d,e),1) else 0)
    f ((a,b,c,d,e),1) = (if a>0 then p!((a-1,b,c,d,e),0) else 0) +
                        (if b>0 then p!((a,b-1,c,d,e),2) else 0)
    f ((a,b,c,d,e),2) = (if b>0 then p!((a,b-1,c,d,e),1) else 0) +
                        (if c>0 then p!((a,b,c-1,d,e),3) else 0)
    f ((a,b,c,d,e),3) = (if c>0 then p!((a,b,c-1,d,e),2) else 0) +
                        (if d>0 then p!((a,b,c,d-1,e),4) else 0)
    f ((a,b,c,d,e),4) = (if d>0 then p!((a,b,c,d-1,e),3) else 0) +
                        (if e>0 then p!((a,b,c,d,e-1),0) else 0)

-- (r,(a,b,c,d,e))
-- r = current orientation
-- a,b,c,d,e = how many previous moves in directions 1,2,3,4,5
path_count_memo :: Int -> (Dir,(Int,Int,Int,Int,Int)) -> Integer
path_count_memo l = p
  where
    p = memoize ((A,(0,0,0,0,0)), (E,(l,l,l,l,l))) f
    f (A,(0,0,0,0,0)) = 1
    f (A,(a,b,c,d,e)) = (if e>0 then p (E,(a,b,c,d,e-1)) else 0) +
                        (if a>0 then p (B,(a-1,b,c,d,e)) else 0)
    f (B,(a,b,c,d,e)) = (if a>0 then p (A,(a-1,b,c,d,e)) else 0) +
                        (if b>0 then p (C,(a,b-1,c,d,e)) else 0)
    f (C,(a,b,c,d,e)) = (if b>0 then p (B,(a,b-1,c,d,e)) else 0) +
                        (if c>0 then p (D,(a,b,c-1,d,e)) else 0)
    f (D,(a,b,c,d,e)) = (if c>0 then p (C,(a,b,c-1,d,e)) else 0) +
                        (if d>0 then p (E,(a,b,c,d-1,e)) else 0)
    f (E,(a,b,c,d,e)) = (if d>0 then p (D,(a,b,c,d-1,e)) else 0) +
                        (if e>0 then p (A,(a,b,c,d,e-1)) else 0)
{-
-- (r,(a,b,c,d,e))
-- r = current orientation
-- a,b,c,d,e = how many previous visits to states A,B,C,D,E
path_count_memo' :: Int -> (Dir,(Int,Int,Int,Int,Int)) -> Integer
path_count_memo' l = p
  where
    p = memoize ((A,(0,0,0,0,0)), (E,(l,l,l,l,l))) f
    f (A,(0,0,0,0,0)) = 1
    f (A,(a,b,c,d,e)) = (if e>0 then p (E,(a,b,c,d,e-1)) else 0) +
                        (if b>0 then p (B,(a,b-1,c,d,e)) else 0)
    f (B,(a,b,c,d,e)) = (if a>0 then p (A,(a-1,b,c,d,e)) else 0) +
                        (if c>0 then p (C,(a,b,c-1,d,e)) else 0)
    f (C,(a,b,c,d,e)) = (if b>0 then p (B,(a,b-1,c,d,e)) else 0) +
                        (if d>0 then p (D,(a,b,c,d-1,e)) else 0)
    f (D,(a,b,c,d,e)) = (if c>0 then p (C,(a,b,c-1,d,e)) else 0) +
                        (if e>0 then p (E,(a,b,c,d,e-1)) else 0)
    f (E,(a,b,c,d,e)) = (if d>0 then p (D,(a,b,c,d-1,e)) else 0) +
                        (if a>0 then p (A,(a-1,b,c,d,e)) else 0)

-- (r,(a,b,c,d,e))
-- r = current orientation
-- a,b,c,d,e = how many visits (including now) to states A,B,C,D,E
path_count_memo3 :: Int -> (Dir,(Int,Int,Int,Int,Int)) -> Integer
path_count_memo3 l = p
  where
    p = memoize ((A,(0,0,0,0,0)), (E,(l,l,l,l,l))) f
    f (A,(1,0,0,0,0)) = 1
    f (A,(a,b,c,d,e)) =
      if a>0 then p (E,(a-1,b,c,d,e)) + p (B,(a-1,b,c,d,e)) else 0
    f (B,(a,b,c,d,e)) =
      if b>0 then p (A,(a,b-1,c,d,e)) + p (C,(a,b-1,c,d,e)) else 0
    f (C,(a,b,c,d,e)) =
      if c>0 then p (B,(a,b,c-1,d,e)) + p (D,(a,b,c-1,d,e)) else 0
    f (D,(a,b,c,d,e)) =
      if d>0 then p (C,(a,b,c,d-1,e)) + p (E,(a,b,c,d-1,e)) else 0
    f (E,(a,b,c,d,e)) =
      if e>0 then p (D,(a,b,c,d,e-1)) + p (A,(a,b,c,d,e-1)) else 0
-}

--prob208 l = path_count l ! (0,(l,l,l,l,l))
prob208 l = path_count_memo l (A,(l,l,l,l,l))

{-
 1: 2
 2: 12
 3: 188
 4: 3400
 5: 70932
 6: 1590624
 7: 37697984
 8: 929034936
 9: 23602480100
10: 614147964232
11: 16294372872648
12: 439328643919048
13: 12006686155322432
14: 331951449665644800
15: 9269537950215138048
-}

main :: IO String
main = return $ show $ prob208 14
-- 331951449665644800