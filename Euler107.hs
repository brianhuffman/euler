module Euler107 where
import qualified Data.IntSet as IntSet
import qualified SortedList as S
import Data.List (sort, insert)
import Data.Array
import Data.Maybe
import Data.Char (isDigit)

------------------------------------------------------------------------------
-- 107. Determining the most efficient way to connect the network.
{-
The following undirected network consists of seven vertices and twelve edges
with a total weight of 243.

       B--20---E
      / \     / \
    16  17  18  11
    /     \ /     \
   A--21---D--23---G
    \     / \     /
    12  28  19  27
      \ /     \ /
       C--31---F

The same network can be represented by the matrix below.
    	A	B	C	D	E	F	G
A	-	16	12	21	-	-	-
B	16	-	-	17	20	-	-
C	12	-	-	28	-	31	-
D	21	17	28	-	18	19	23
E	-	20	-	18	-	-	11
F	-	-	31	19	-	-	27
G	-	-	-	23	11	27	-

However, it is possible to optimise the network by removing some edges and
still ensure that all points on the network remain connected. The network
which achieves the maximum saving is shown below. It has a weight of 93,
representing a saving of 243 - 93 = 150 from the original network.

       B       E
      / \     / \
    16  17  18  11
    /     \ /     \
   A       D       G
    \       \
    12      19
      \       \
       C       F

Using network.txt, a 6K text file containing a network with forty vertices,
and given in matrix form, find the maximum saving which can be achieved by
removing redundant edges whilst ensuring that the network remains connected.
-}

type Node = Int
type Weight = Int
type Graph = Array (Node, Node) (Maybe Weight)

networkTxt :: IO Graph
networkTxt = readFile "network.txt" >>= return .
  listArray ((1,1),(40,40)) . concatMap parse . lines
  where
    parse [] = []
    parse ('-':s) = Nothing : parse s
    parse (',':s) = parse s
    parse ('\r':s) = parse s
    parse s = let (x,s') = span isDigit s in Just (read x) : parse s'

{-
Prim's algorithm (very similar to algorithm used in Problem 83)

State:
1) Total weight of subgraph so far :: Weight
2) Set of nodes reachable so far :: Node Set
3) List of edges from reachable to unreachable nodes :: [(Weight, Node, Node)]

Recursively add edge with least weight, and update state,
until list of edges is empty.
-}

minimal_spanning_weight :: Graph -> Weight
minimal_spanning_weight g = step 0 nodes0 edges0
  where
    bnds@((n0,_),(n1,_)) = bounds g
    nodes0 = IntSet.singleton n0
    edges0 = new_edges nodes0 n0
    new_edges nodes i = sort
      [ (w, i, j) |
        j <- range (n0, n1),
        not (IntSet.member j nodes),
        w <- maybeToList (g ! (i, j)) ]
    step t nodes [] = t
    step t nodes ((w,i,j) : edges) = step t' nodes' edges''
      where
        t' = t + w
        nodes' = IntSet.insert j nodes
        edges' = filter (\(_,_,j') -> j' /= j) edges
        edges'' = foldr insert edges' (new_edges nodes' j)

total_weight :: Graph -> Weight
total_weight g = sum
  [ w |
    (i, j) <- indices g,
    i < j,
    w <- maybeToList (g ! (i, j)) ]

prob107 :: Graph -> Weight
prob107 net = total_weight net - minimal_spanning_weight net

main :: IO String
main = networkTxt >>= return . show . prob107
-- 259679

