module Permutation where

permutations [] = return []
permutations ys = do
  (x, xs) <- remove1 ys
  xs' <- permutations xs
  return (x:xs')

subsets [] = [[]]
subsets (x:xs) = map (x:) xss ++ xss
  where xss = subsets xs

subseqs_len 0 _ = [[]]
subseqs_len n [] = []
subseqs_len n (x:xs) =
  map (x:) (subseqs_len (n-1) xs) ++ subseqs_len n xs

remove1 [] = []
remove1 (x:xs) = (x,xs) : map (\(y,ys) -> (y,x:ys)) (remove1 xs)

partitionPairs [] = [([],[])]
partitionPairs (x:xs) =
  map (\(ys,zs) -> (x:ys,zs)) ps ++
  map (\(ys,zs) -> (ys,x:zs)) ps
  where ps = partitionPairs xs

partitionLists :: [a] -> [[[a]]]
partitionLists [] = [[]]
partitionLists (x:xs) =
  do (ys,zs) <- partitionPairs xs
     ls <- partitionLists zs
     return ((x:ys):ls)
