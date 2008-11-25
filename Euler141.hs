module Euler141 where
import EulerLib
import Primes
import qualified Data.Set as Set
import Monad
import List
import Data.Array.Unboxed
import Bits

------------------------------------------------------------------------------
-- 141. Investigating progressive numbers, n, which are also square.
{-
n = k^2 = q*d + r  (r < d)
{d,q,r} are a geometric sequence
possible orderings:
  q < r < d
  r < q < d
  r < d < q

integer geometric sequences must take the form
  (aac, abc, bbc) for a < b

  (q,r,d) = (aac, abc, bbc)
  q*d + r = (aac)(bbc) + abc = (abc)^2 + abc = (abc)(abc + 1) (NOT SQUARE)

  (r,q,d) = (aac, abc, bbc)
  q*d + r = (abc)(bbc) + aac = abbbcc + aac = (ac)(bbbc + a)

  (r,d,q) = (aac, abc, bbc)
  q*d + r = (bbc)(abc) + aac = abbbcc + aac = (ac)(bbbc + a)

  (aac)(aac + 1) = aaaacc + aac < abbbcc + aac = k^2
  (aac)(aac + 1) < k^2
  (aac)^2 < k^2
  aac < k

WLOG assume r < d < q
r = aac
d = abc
q = bbc
n = k^2 = aac + abbbcc = ac (a + bbbc)

r(n-r) = rdq = ddd

(4ab^3)k^2 = (4ab^3)(a^2c + ab^3c^2)
4ab^3k^2 = 4a^3b^3c + 4a^2b^6c^2
4ab^3k^2 = (2ab^3c)^2 + 2(2ab^3c)a^2
4ab^3k^2 = (2ab^3c + a^2)^2 - a^4
-}

{-
Testing for cubes:
mod 7: [0,1,6]
mod 9: [0,1,8]
mod 13: [0,1,5,8,12]
-}

-- all progressive numbers up to m
progressives m =
  [ n |
    a <- takeWhile (\a -> a^4 < m) [1 ..],
    let a2 = a^2,
    let b3max = (m - a2) `div` a,
    b <- takeWhile (\b -> b^3 <= b3max) [a+1 ..],
    gcd a b == 1,
    let ab3 = a * b^3,
    let ns = [ (a2 + ab3*c)*c | c <- [1 ..] ],
    n <- takeWhile (<= m) ns ]

maybe_square = \n -> a ! (n .&. 255)
  where
    squares = [ (n^2 .&. 255, True) | n <- [0 .. 255] ]
    a :: UArray Integer Bool
    a = accumArray (||) False (0,255) squares

-- perfect square progressives up to m
square_progressives m =
  filter is_square $
  filter maybe_square $
  progressives m
  where
    is_square n = Set.member n square_set
    square_set = Set.fromList
      (takeWhile (<= m) [ n^2 | n <- [1 ..] ])

prob141 :: Integer -> Integer
prob141 m = sum $ nub $ square_progressives (m-1)

main :: IO String
main = return $ show $ prob141 (10^12)
-- 878454337159

--------------------------------------------------------------

prob141c m =
  [ a*c*(b3*c + a) |
    b <- takeWhile (\b -> b^3 < m) [1 ..],
    let b3 = b^3,
    let amax = m `div` b3,
    a <- [1 .. amax],
    c <- [1 .. amax `div` a] ]

list_divisors_of_square n =
  list_divisors_of_pf [ (p,2*e) | (p,e) <- prime_factorization n ]

divM m n =
  case divMod m n of
    (q,0) -> return q
    _ -> mzero
{-
cube_rootM x =
  either (const mzero) return $
  binary_search (\n -> compare x (n^3))
-}
cubeSet = Set.fromList [ n^3 | n <- [1 .. 9999] ]

-- exists a < b, c such that k^2 = (ac)(bbbc + a) ?
prob141a k = do
  let fs = takeWhile (< k) (list_divisors_of_square k)
  a <- fs
  c <- fs
  guard (a*a*c < k)
  q <- divM (k^2) (a*c)
  r <- divM (q-a) c
  guard (Set.member r cubeSet)
  -- b <- S.intersectBy (\r b -> compare r (b^3)) [r] [a+1 ..]
  return (a,c)

prob141b = filter (not . null . prob141a) [1 .. 1000000]
{-[3, 102, 130, 312, 759, 2496, 2706, 3465, 6072, 6111, 8424, 14004, 16005, 36897, 37156, 92385, 98640, 112032, 117708, 128040, 351260, 378108, 740050]-}

-- 3^2 = 1 + 2 * 4           (1,2,1)
-- 102^2 = 36 + 72 * 144     (1,2,36)
-- 130^2 = 25 + 75 * 225     (1,3,25)
-- 312^2 = 8 + 92 * 1058     (2,23,2)
-- 759^2 = 81 + 360 * 1600   (9,40,1)
-- 2496^2 = 512 + 1472 * 4232  (8,23,8)
-- 2706^2 = 1936 + 2420 * 3025 (4,5,121)

-- prob141 = sum (map square prob141b)

-- main :: Int
-- main = prob141

ks = [3, 102, 130, 312, 759, 2496, 2706, 3465, 6072, 6111, 8424, 14004, 16005, 36897, 37156, 92385, 98640, 112032, 117708, 128040, 351260, 378108, 740050]

ns = map (^2) ks