module Euler244 where
import Data.Bits
import Data.Array.Unboxed
import EulerLib (funArray)

{-
Problem 244
Sliders

09 May 2009

You probably know the game Fifteen Puzzle. Here, instead of numbered
tiles, we have seven red tiles and eight blue tiles.

A move is denoted by the uppercase initial of the direction (Left,
Right, Up, Down) in which the tile is slid, e.g. starting from
configuration (S), by the sequence LULUR we reach the configuration
(E):

(S)
-*OO
**OO
**OO
**OO

(E)
**OO
*OOO
*-*O
**OO

For each path, its checksum is calculated by (pseudocode):
checksum = 0
checksum = (checksum × 243 + m_(1)) mod 100 000 007
checksum = (checksum × 243 + m_(2)) mod 100 000 007
   …
checksum = (checksum × 243 + m_(n)) mod 100 000 007

where m_(k) is the ASCII value of the k^(th) letter in the move
sequence and the ASCII values for the moves are:

L	76
R	82
U	85
D	68

For the sequence LULUR given above, the checksum would be 19761398.

Now, starting from configuration (S), find all shortest ways to reach
configuration (T).

(S)
-*OO
**OO
**OO
**OO

(T)	
-O*O
O*O*
*O*O
O*O*

What is the sum of all checksums for the paths having the minimal
length?

-}

data Dir = L | R | U | D
  deriving (Eq, Show)

{-

How many states are there?

Represented as bits,
10^19 = 524288

-}

type Pos = Int

-- 0 1 2 3
-- 4 5 6 7
-- 8 9 A B
-- C D E F

moves :: Pos -> [(Dir, Pos)]
moves 0x0 = [          (L, 0x1),           (U, 0x4)]
moves 0x1 = [(R, 0x0), (L, 0x2),           (U, 0x5)]
moves 0x2 = [(R, 0x1), (L, 0x3),           (U, 0x6)]
moves 0x3 = [(R, 0x2),                     (U, 0x7)]
moves 0x4 = [          (L, 0x5), (D, 0x0), (U, 0x8)]
moves 0x5 = [(R, 0x4), (L, 0x6), (D, 0x1), (U, 0x9)]
moves 0x6 = [(R, 0x5), (L, 0x7), (D, 0x2), (U, 0xA)]
moves 0x7 = [(R, 0x6),           (D, 0x3), (U, 0xB)]
moves 0x8 = [          (L, 0x9), (D, 0x4), (U, 0xC)]
moves 0x9 = [(R, 0x8), (L, 0xA), (D, 0x5), (U, 0xD)]
moves 0xA = [(R, 0x9), (L, 0xB), (D, 0x6), (U, 0xE)]
moves 0xB = [(R, 0xA),           (D, 0x7), (U, 0xF)]
moves 0xC = [          (L, 0xD), (D, 0x8)          ]
moves 0xD = [(R, 0xC), (L, 0xE), (D, 0x9)          ]
moves 0xE = [(R, 0xD), (L, 0xF), (D, 0xA)          ]
moves 0xF = [(R, 0xE),           (D, 0xB)          ]

transitions :: State -> [(Dir, State)]
transitions (p, grid) =
  [ (d, (p', swapBits p p' grid)) | (d, p') <- moves p ]

type Grid = Int

writeBit :: Bits a => a -> Int -> Bool -> a
writeBit x i True = setBit x i
writeBit x i False = clearBit x i

swapBits :: Pos -> Pos -> Grid -> Grid
swapBits i j x = writeBit (writeBit x i jBit) j iBit
  where
    iBit = testBit x i
    jBit = testBit x j

type State = (Pos, Grid)

type Arr = UArray State Int

bnds :: (State, State)
bnds = ((0x0, 0x0000), (0xf, 0xffff))

startState :: State
startState = (0x0, 0x3333)

finalState :: State
finalState = (0x0, 0xa5a5)

initShortPath :: Arr
initShortPath = accumArray (const id) 9999 bnds [(startState, 0)]

nextShortPath :: Arr -> Arr
nextShortPath a = a'
  where
    a' = listArray bnds (map f (range bnds)) 
    f state = min (a ! state) $
      1 + minimum [ a ! state' | (_, state') <- transitions state ]

shortPath :: Arr
shortPath = iterate nextShortPath initShortPath !! 48

shortest :: Int
shortest = shortPath ! (0x0, 0xa5a5)

-- minimum path length is 32.

initCountPath :: Arr
initCountPath = accumArray (const id) 0 bnds [(startState, 1)]

nextCountPath :: Arr -> Arr
nextCountPath a = a'
  where
    a' = listArray bnds (map f (range bnds)) 
    f (p, grid) =
      sum [ a ! (p', swapBits p p' grid) | (_,p') <- moves p ]

countPath :: Arr
countPath = iterate nextCountPath initCountPath !! 32

count :: Int
count = countPath ! finalState

-- 1 path from startState to finalState in 32 moves.

bestpath :: [Dir]
bestpath =
  [L,L,U,R,R,D,L,L,L,U,R,R,D,L,U,U,R,U,L,D,L,U,R,D,R,R,U,L,D,D,R,D]

checksum :: [Dir] -> Integer
checksum = foldl f 0
  where
    f x d = (x * 243 + ascii d) `mod` 100000007
    ascii L = 76
    ascii R = 82
    ascii U = 85
    ascii D = 68

main :: IO String
main = return $ show $ checksum bestpath

answer :: String
answer = "96356848"

-- 48077623 WRONG!

{-
0,1,2,6,5,4,0,1,2,3,7,6,5,1,2,6,a,9,d,e,a,b,f,e,a,9,8,c,d,9,5,4,0
[L,L,U,R,R,D,L,L,L,U,R,R,D,L,U,U,R,U,L,D,L,U,R,D,R,R,U,L,D,D,R,D]


0 1 2 3
4 5 6 7
8 9 a b
c d e f

-*OO
**OO
**OO
**OO
----R,R,D,L
*OOO
*-*O
**OO
**OO
----L,U,R,R
OO-O
***O
**OO
**OO
----R,D,L,L
OOOO
*-**
**OO
**OO
----U,R,D,D
OO*O
*OO*
**-O
**OO
----L,D,R,U
OO*O
*OO*
**-O
*O*O
----R,D,L,U
OO*O
*OO*
**-O
*OO*
----L,L,D,R
OO*O
*OO*
***O
O-O*
----U,U,L,U
-O*O
O*O*
*O*O
O*O*

-}


