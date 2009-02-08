module Euler118 where
import Permutation
import Primes
import Data.Bits
import Data.Array
import Data.Char (digitToInt)

{-
Problem 118
Sets of primes using all digits.

24 March 2006

Using all of the digits 1 through 9 and concatenating them freely to
form decimal integers, different sets can be formed. Interestingly
with the set {2,5,47,89,631}, all of the elements belonging to it are
prime.

How many distinct sets containing each of the digits one through nine
exactly once contain only prime elements?
-}

-- 383 "1234567_9"
-- 447 "123456_89"
-- 495 "1234_6789"
-- 503 "123_56789"
-- 509 "1_3456789"
-- 510 "_23456789"
-- 511 "123456789"

-- Key represents a set of digits
-- bit n represents presence of digit n
type Key = Int

key :: String -> Key
key ds = sum (map enc ds)
  where enc d = bit (digitToInt d - 1)

str :: Key -> String
str = s '1'
  where
     s c 0 = ""
     s c k = let cs = s (succ c) (k`div`2)
             in if odd k then c : cs else cs
     
-- count_key ! k = number of primes represented by key k
count_key :: Array Key Int
count_key = listArray (1, 511) (map (prime_perms . str) [1 .. 511])
  where
    prime_perms [d] = if test_prime (digitToInt d) then 1 else 0
    prime_perms ds
      | mult3 ds  = 0
      | otherwise = length (filter (test_prime . read) (perms ds))
    mult3 ds = sum (map digitToInt ds) `mod` 3 == 0

-- only include permutations that end in 1,3,7,9.
perms [x]
  | x `elem` "1379" = return [x]
  | otherwise       = []
perms ys = do
  (x, xs) <- remove1 ys
  xs' <- perms xs
  return (x:xs')

test_prime :: Int -> Bool
test_prime n = miller_rabin (toInteger n)

prime_partitions :: String -> Int
prime_partitions [] = 1
prime_partitions (d : ds) = sum
  [ x * y |
    (ds1, ds2) <- partitionPairs ds,
    let x = count_key ! key (d : ds1),
    let y = prime_partitions ds2 ]

main :: IO String
main = return $ show $ prime_partitions "123456789"
-- main = return $ show $ sum $ elems $ count_key
-- 44680
