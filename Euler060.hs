module Euler060 where
import EulerLib
import Primes
import qualified SortedList as S
import List

------------------------------------------------------------------------------
-- 60. Find a set of five primes for which any two primes concatenate to produce another prime.
{-
The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes
and concatenating them in any order the result will always be prime. For
example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these
four primes, 792, represents the lowest sum for a set of four primes with
this property.

Find the lowest sum for a set of five primes for which any two primes
concatenate to produce another prime.
-}

{-
[(t,xs,yss)]
where t = sum xs
      xs = list of elements, all compatible with each other
      ys = stream of primes, greater than all in xs, compatible with xs
-}

-- Chunked lists

type CList = [[I]]

chunk_size :: I
chunk_size = 1000

chunk :: [I] -> CList
chunk xs = f chunk_size xs
  where
    f s xs =
      let (ys, zs) = span (<s) xs
      in ys : f (s+chunk_size) zs

unchunk :: CList -> [I]
unchunk = concat

cinter :: CList -> CList -> CList
cinter = zipWith S.intersect

--------------------------------


cat :: I -> I -> I
cat a b = a * ndigits b + b
  where
    ndigits n
      | n < 10^1 = 10^1
      | n < 10^2 = 10^2
      | n < 10^3 = 10^3
      | n < 10^4 = 10^4
      | n < 10^5 = 10^5
      | n < 10^6 = 10^6
      | n < 10^7 = 10^7
      | n < 10^8 = 10^8
      | otherwise = error "cat: argument too big"

compatible :: I -> I -> Bool
compatible x y = is_prime (cat x y) && is_prime (cat y x)

compats :: [(I, [I])]
compats = [ (p, filter (compatible p) ps) | p:ps <- tails primes ]

{-
cliques_1 :: [(Clique, Stream)]
cliques_1 = [ ((p, [p]), ps) | (p, ps) <- compats ]

cliques_next :: [(Clique, Stream)] -> [(Clique, Stream)]
cliques_next = 
-}


cat_is_prime :: (I, I) -> Bool
cat_is_prime = {-memoize ((2,2),(100,10000))-} f
  where f (x,y) = is_prime (cat x y) && is_prime (cat y x)

rem_thd :: (a, b, c) -> (a, b)
rem_thd (t,x,_) = (t,x)

type I = Int
type Clique = (I, [I])
type Stream = [I]
type State = [(I, [I])]

prob60b :: I -> [[Clique]]
prob60b m = map (map fst) (iterate nextset set0)
  where
    ps0 :: [I]
    ps0 = takeWhile (< m) primes
    yss0 :: State
    yss0 = [ (x, filter (compatible x) xs) | x:xs <- tails ps0 ]
    set0 :: [(Clique, State)]
    set0 = [((0, []), yss0)]
    nextset :: [(Clique, State)] -> [(Clique, State)]
    nextset = concatMap sets_from
    inter :: State -> [I] -> State
    inter = S.intersectBy (\(a,_) b -> compare a b)
    sets_from :: (Clique, State) -> [(Clique, State)]
    sets_from ((t,xs),yss) =
      [ ((t+y, y:xs), zss) |
        (y,ys) <- yss,
        let zss = inter yss ys ]

prob60b' :: I -> [[(Clique, State)]]
prob60b' m = iterate nextset set0
  where
    ps0 :: [I]
    ps0 = takeWhile (< m) primes
    yss0 :: State
    yss0 = [ (x, filter (compatible x) xs) | x:xs <- tails ps0 ]
    set0 :: [(Clique, State)]
    set0 = [((0, []), yss0)]
    nextset :: [(Clique, State)] -> [(Clique, State)]
    nextset = concatMap sets_from
    inter :: State -> [I] -> State
    inter = S.intersectBy (\(a,_) b -> compare a b)
    sets_from :: (Clique, State) -> [(Clique, State)]
    sets_from ((t,xs),yss) =
      [ ((t+y, y:xs), zss) |
        (y,ys) <- yss,
        let zss = inter yss ys ]

{-
prob60a = map (map rem_thd) (iterate nextset set1)
  where
    ok (_,y:xs,_) =
      all (\x -> cat_is_prime (x,y)) (reverse xs)
    xs0 = takeWhile (< maxp) primes
    set1 = listrec (\x xs -> (x,[x],xs)) xs0
    nextset = filter ok . S.big_union .
      map (takeWhile (\(t,_,_) -> t <= 34427)) .
      map (\(t,xs,ys) -> listrec (\y ys -> (t+y, (y:xs), ys)) ys)

-- (34427,[18433,12409,2341,1237,7])
-- (26033,[8389,6733,5701,5197,13])
-}

prob60 :: Int -> Clique
prob60 n = f 10000
  where
    f m = case prob60b m !! n of
            [] -> f (2*m)
            xs -> minimum xs
-- prob60 5 = (26033,[8389,6733,5701,5197,13])

main :: IO String
main = return $ show $ fst $ prob60 5
