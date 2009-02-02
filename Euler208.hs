module Euler208 where
import Memoize
import EulerLib (funArray)
import Data.Array

{-
Problem 208
12 September 2008

A robot moves in a series of one-fifth circular arcs (72°), with a
free choice of a clockwise or an anticlockwise arc for each step, but
no turning on the spot.

One of 70932 possible closed paths of 25 arcs starting northward is

LRLLLRRRRLRRLRLLLLRLLRLLL
5554334511122221544433321
EAEDCDEABABCBCBAEDEDCDCBA

Given that the robot starts facing North, how many journeys of 70 arcs
in length can it take that return it, after the final arc, to its
starting position?  (Any arc may be traversed multiple times.)

-}

{-
Directions of movement: 1,2,3,4,5

  * 2 *
 1     3
*       *
  5   4
    *

Possible orientations: A,B,C,D,E  (A = North)

    A   
E-_ | _-B
   `*'
   / \
  D   C

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

Out of 32 possible combinations of parities, only 10 are reachable:
(0,0,0,0,0)
(1,0,0,0,0)
(1,1,0,0,0)
(1,1,1,0,0)
(1,1,1,1,0)
(1,1,1,1,1)
(0,1,1,1,1)
(0,0,1,1,1)
(0,0,0,1,1)
(0,0,0,0,1)

 n  visited states
----------------
 0  1 / 1
 1  10 / 32
 2  65 / 243
 3  276 / 1024
 4  893 / 3125
 5  2216 / 7776
 6  4981 / 16807
 7  9646 / 32768
 8  17825 / 59049
 9  29982 / 100000

-}

type State = (Int, Int, Int, Int, Int)

-- a,b,c,d,e = how many previous moves in directions 1,2,3,4,5
path_count_memo :: Int -> State -> Integer
path_count_memo l = p
  where
    p = memoize ((0,0,0,0,0), (l,l,l,l,l)) f
    f (0,0,0,0,0) = 1
    f (a,b,c,d,e) = (if e>0 then p (e-1,a,b,c,d) else 0) +
                    (if a>0 then p (b,c,d,e,a-1) else 0)

path_count_array :: Int -> Array State Integer
path_count_array l = p
  where
    p = funArray ((0,0,0,0,0), (l,l,l,l,l)) f
    f (0,0,0,0,0) = 1
    f (a,b,c,d,e) = (if e>0 then p!(e-1,a,b,c,d) else 0) +
                    (if a>0 then p!(b,c,d,e,a-1) else 0)

prob208 :: Int -> Integer
--prob208 l = path_count_memo l (l,l,l,l,l)
prob208 l = path_count_array l ! (l,l,l,l,l)

main :: IO String
main = return $ show $ prob208 14

answer :: String
answer = "331951449665644800"
