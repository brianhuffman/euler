module Euler060 where
import EulerLib
import Primes
import qualified SortedList as S
import Data.List (tails)

{-
Problem 60
02 January 2004

The primes 3, 7, 109, and 673, are quite remarkable. By taking any two
primes and concatenating them in any order the result will always be
prime. For example, taking 7 and 109, both 7109 and 1097 are
prime. The sum of these four primes, 792, represents the lowest sum
for a set of four primes with this property.

Find the lowest sum for a set of five primes for which any two primes
concatenate to produce another prime.

-}

type Z = Int

cat :: Z -> Z -> Z
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

compatible :: Z -> Z -> Bool
compatible x y = is_prime (cat x y) && is_prime (cat y x)

type Clique = (Z, [Z])
{-
(t, xs) :: Clique
where
  t = sum xs
  xs = descending list of primes, all compatible with each other
-}


type State = [(Z, [Z])]
{-
[(x,ys)] :: State
where
  x = prime
  ys = increasing stream of primes, all compatible with x
-}

prob60_upto :: Z -> [[Clique]]
prob60_upto m = map (map fst) (iterate nextset set0)
  where
    ps0 :: [Z]
    ps0 = takeWhile (< m) primes
    yss0 :: State
    yss0 = [ (x, filter (compatible x) xs) | x:xs <- tails ps0 ]
    set0 :: [(Clique, State)]
    set0 = [((0, []), yss0)]
    nextset :: [(Clique, State)] -> [(Clique, State)]
    nextset = concatMap sets_from
    sets_from :: (Clique, State) -> [(Clique, State)]
    sets_from ((t,xs),yss) =
      [ ((t+y, y:xs), zss) |
        (y,ys) <- yss,
        let zss = inter yss ys ]
    inter :: State -> [Z] -> State
    inter = S.intersectBy (\(a,_) b -> compare a b)

{-
(34427,[18433,12409,2341,1237,7])
(26033,[8389,6733,5701,5197,13])
-}

prob60 :: Int -> Clique
prob60 n = f 10000
  where
    f m = case prob60_upto m !! n of
            [] -> f (2*m)
            xs -> minimum xs
-- prob60 5 = (26033,[8389,6733,5701,5197,13])

main :: IO String
main = return $ show $ fst $ prob60 5

answer :: String
answer = "26033"
