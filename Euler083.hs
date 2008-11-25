module Euler083 where
import EulerLib
import Array

------------------------------------------------------------------------------
-- 83. Find the minimal path sum from the top left to the bottom right by moving left, right, up, and down.

type Node = (Int, Int)

type Matrix = Array Node Int

test :: Matrix
test = listArray ((0,0),(4,4))
 [131, 673, 234, 103,  18,
  201,  96, 342, 965, 150,
  630, 803, 746, 422, 111,
  537, 699, 497, 121, 956,
  805, 732, 524,  37, 331]

---------------------------------------------------------------------
{-
Use Dijkstra's algorithm for minimal paths.

In each step, maintain arrays
  seen :: Array Node Bool
  dist :: Array Node Int
and a sorted list
  new :: [(Node, Int)]

We only add one node at a time.

When adding a node:
1. Add the node to the visited set, and minimal array.
2. Make a list of all possible transitions
   from the new node to unvisited nodes.
3. For each transition, calculate the new
   minimal cost to reach there.
4. Add those (node, minimal cost) pairs to the queue.

While the queue is non-empty, add the next node.

We are done as soon as we add the destination node.
-}

insert_dist :: (Node, Int) -> [(Node, Int)] -> [(Node, Int)]
insert_dist (m, x) [] = [(m, x)]
insert_dist (m, x) ((n, y):ns)
  | x <= y = (m, x) : (n, y) : ns
  | otherwise = (n, y) : insert_dist (m, x) ns

minimal_path_sum :: Matrix -> Int
minimal_path_sum e = step dist0 [(n0, e!n0)]
  where
    bnds@(n0, n1) = bounds e
    next (r,c) = [(r-1,c), (r,c-1), (r+1,c), (r,c+1)]
    neighbors = filter (inRange bnds) . next
    dist0 = accumArray const Nothing bnds []
    step dist ((n, x):ns) = if n == n1 then x else step dist' ns''
      where
        dist' = dist // [(n, Just x)]
        ns' = filter ((/= n) . fst) ns
        new = [ (n', x + e!n') | n' <- neighbors n, dist'!n' == Nothing ]
        ns'' = foldr insert_dist ns' new

matrixTxt :: IO Matrix
matrixTxt = do 
  s <- readFile "matrix.txt"
  let ls = lines s
  let es = concatMap (\s -> read ("[" ++ s ++ "]")) ls
  return (listArray ((0,0),(79,79)) es)

main :: IO String
main = matrixTxt >>= return . show . minimal_path_sum
-- 425185
