module Euler237 where
import Data.Array
import EulerLib (funArray)

{-
Problem 237
Tours on a 4xn Playing Board

21 March 2009

Let T(n) be the number of tours over a 4 × n playing board such that:

    * The tour starts in the top left corner.
    * The tour consists of moves that are up, down, left, or right one
      square.
    * The tour visits each square exactly once.
    * The tour ends in the bottom left corner.

The diagram shows one tour over a 4 × 10 board:

  o---+---+   +---+---+---+---+   +---+
          |   |               |   |   |
  +---+   +---+   +---+---+---+   +   +
  |   |           |               |   |
  +   +   +---+   +---+---+---+---+   +
  |   |   |   |                       |
  o   +---+   +---+---+---+---+---+---+

T(10) is 2329. What is T(10^(12)) modulo 10^(8)?

-}


{-
T(1) = 1
T(2) = 1
T(3) = 4

Splitting the board in half, how many kinds of transitions
between the two sides are there?

  A      B      C      D      E      F      G      H

--1->  --1->  --3->  --1->  --1->    |      |      |
<-2--  <-4--  <-2--    |    <-2--  --1->    |      |
--3->  --3->  --1->    |      |    <-2--  --1->    |
<-4--  <-2--  <-4--  <-2--    |      |    <-2--    |

Which combinations of transitions can be one square apart?

A-A                 A-E       A-G  A-H
     B-B       B-D
          C-C  C-D
D-A                 D-E  D-F  D-G  D-H
     E-B       E-D
               F-D
          G-C  G-D

There are 18 such combinations:

A-A, A-E, A-G, A-H, B-B, B-D, C-C, C-D, D-A
___  ___  _    _    ___  ___  ___  ___  ___
___  ___  _|   _|   ___  _    ___  _      _
___  _    ___  _    ___  _|   ___  _|    |_
___  _|   ___  _|   ___  ___  ___  ___  ___

D-E, D-F, D-G, D-H, E-B, E-D, F-D, G-C, G-D
___  _    _    _    ___  ___    _    _    _
  _   |_   |    |   ___  _    _|    |_   |
 |     _   |_   |     _   |   _    ___  _|
_|   _|   ___  _|    |_   |_   |_  ___  ___

D-F-D   D-G-D   D-E-D
_   _   _   _   _____
 |_|     | |      _
  _      |_|     | |
_| |_   _____   _| |_

D-A-G-D   D-A-G-C-D   D-G-C-D
___   _   ___   ___   _   ___
  _| |      _| |_      | |_
 |___|     |_____|     |___|
_______   _________   _______

D-A-E-D   D-A-E-B-D   D-E-B-D
_______   _________   _______
  ___       _____       ___
 |_  |     |_   _|     |  _|
___| |_   ___| |___   _| |___

D-A-H   D-H
___     _
  _|     |
 |_      |
___|    _|


Regular expression for possible sequences of transitions:
D((A*GC* + A*EB* + F)D)*A*H

Grammar productions:
DH
DH -> DAH
Dx -> DEDx
Dx -> DFDx
Dx -> DGDx
DAx -> DAAx
DEx -> DEBx
DEx -> DAEx
DGx -> DGCx
DGx -> DAGx
DAx -> DAAx

-}

data Transition = A | B | C | D | E | F | G | H
  deriving (Show, Eq, Ord, Enum, Ix)

type Combination = (Transition, Transition)

bnds :: (Combination, Combination)
bnds = ((A, A), (H, H))

size1 :: [Combination]
size1 =
  [(A,A),(A,E),(A,G),(A,H),(B,B),(B,D),(C,C),(C,D),(D,A)
  ,(D,E),(D,F),(D,G),(D,H),(E,B),(E,D),(F,D),(G,C),(G,D)]

type Table = Array Combination Integer

table1 :: Table
table1 = accumArray (+) 0 bnds [ (c, 1) | c <- size1 ]

combine :: Table -> Table -> Table
combine tabL tabR = funArray bnds f
  where f (a,c) = sum [ tabL!(a,b) * tabR!(b,c) | b <- [A .. G] ]
-- H is not needed, since it never occurs in the middle

table2 :: Table
table2 = combine table1 table1

table :: Integer -> Table
table = fst . tables

-- tables n = (tables n, tables n+1)
tables :: Integer -> (Table, Table)
tables 1 = (table1, table2)
tables k = (combine tabL1 tabR1, combine tabL2 tabR2)
  where
    (tab1, tab2) = tables (k `div` 2)
    (tabL1, tabR1, tabL2, tabR2) =
      if even k then (tab1, tab1, tab1, tab2)
                else (tab1, tab2, tab2, tab2)

reduce_table :: Integer -> Table -> Table
reduce_table n = fmap (`mod` n)

table_mod :: Integer -> Integer -> Table
table_mod n = fst . tables_mod n

tables_mod :: Integer -> Integer -> (Table, Table)
tables_mod n 1 = (table1, table2)
tables_mod n k = (combine' tabL1 tabR1, combine' tabL2 tabR2)
  where
    (tab1, tab2) = tables_mod n (k `div` 2)
    (tabL1, tabR1, tabL2, tabR2) =
      if even k then (tab1, tab1, tab1, tab2)
                else (tab1, tab2, tab2, tab2)
    combine' tabL tabR = reduce_table n (combine tabL tabR)

prob237 :: Integer -> Integer -> Integer
prob237 n k = table_mod n k ! (D,H)

main :: IO String
main = return $ show $ prob237 (10^8) (10^12)

answer :: String
answer = "15836928"
