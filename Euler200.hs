module Euler200 where
import qualified SortedList as S
import Primes

squbes :: [Integer]
squbes = S.big_union [ [ p^2 * q^3 | p <- primes, p /= q ] | q <- primes ]

change1 :: Integer -> [Integer]
change1 n = map read (f (show n))
  where
    f [] = []
    f (x:xs) =
      [ x':xs | x' <- ['0'..'9'], x' /= x ] ++
      [ x:xs' | xs' <- f xs ]

prime_proof :: Integer -> Bool
prime_proof n = not $ any is_prime $ change1 n

contains200 :: Integer -> Bool
contains200 n = f (show n)
  where
    f ('2':'0':'0':_) = True
    f (x:xs) = f xs
    f [] = False

prime_proof_squbes_200 =
  filter prime_proof $
  filter contains200 $  -- much faster, do this first.
  squbes

main :: IO String
main = return $ show $ prime_proof_squbes_200 !! 199
