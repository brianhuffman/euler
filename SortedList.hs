module SortedList where
import qualified List

nubBy f [] = []
nubBy f [x] = [x]
nubBy f (x:xs@(y:ys)) =
  if f x y then nubBy f xs else x : nubBy f xs

elemBy f x [] = False
elemBy f x (y:ys) =
  case f x y of
    LT -> elemBy f x ys
    EQ -> True
    GT -> False

insertBy f x [] = [x]
insertBy f x (y:ys) =
  case f x y of
    EQ -> x : ys
    LT -> x : y : ys
    GT -> y : insertBy f x ys

unionBy f xs [] = xs
unionBy f [] ys = ys
unionBy f (x:xs) (y:ys) =
  case f x y of
    EQ -> x : unionBy f xs ys
    LT -> x : unionBy f xs (y:ys)
    GT -> y : unionBy f (x:xs) ys

mergeBy f xs [] = xs
mergeBy f [] ys = ys
mergeBy f (x:xs) (y:ys) =
  case f x y of
    EQ -> x : y : mergeBy f xs ys
    LT -> x : mergeBy f xs (y:ys)
    GT -> y : mergeBy f (x:xs) ys

intersectBy f xs [] = []
intersectBy f [] ys = []
intersectBy f (x:xs) (y:ys) =
  case f x y of
    EQ -> x : intersectBy f xs (y:ys)
    LT -> intersectBy f xs (y:ys)
    GT -> intersectBy f (x:xs) ys

deleteFirstsBy f xs [] = xs
deleteFirstsBy f [] ys = []
deleteFirstsBy f (x:xs) (y:ys) =
  case f x y of
    LT -> x : deleteFirstsBy f xs (y:ys)
    EQ -> deleteFirstsBy f xs ys
    GT -> deleteFirstsBy f (x:xs) ys

nub :: Eq a => [a] -> [a]
nub = nubBy (==)

elem :: Ord a => a -> [a] -> Bool
elem = elemBy compare

insert :: Ord a => a -> [a] -> [a]
insert = insertBy compare

union :: Ord a => [a] -> [a] -> [a]
union = unionBy compare

merge :: Ord a => [a] -> [a] -> [a]
merge = mergeBy compare

intersect :: Ord a => [a] -> [a] -> [a]
intersect = intersectBy compare

deleteFirsts :: Ord a => [a] -> [a] -> [a]
deleteFirsts = deleteFirstsBy compare

elemOf f = elemBy (\x y -> compare (f x) (f y))
unionOf f = unionBy (\x y -> compare (f x) (f y))
intersectOf f = intersectBy (\x y -> compare (f x) (f y))

big_union [] = []
big_union ([] : xss) = big_union xss
big_union ((x:xs):xss) = x : union xs (big_union xss)

big_merge [] = []
big_merge ([] : xss) = big_merge xss
big_merge ((x:xs):xss) = x : merge xs (big_merge xss)
