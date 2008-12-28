module Euler225 where
import qualified SortedList as S

{-
Problem 225
26 December 2008

The sequence 1, 1, 1, 3, 5, 9, 17, 31, 57, 105, 193, 355, 653, 1201
...  is defined by T_(1) = T_(2) = T_(3) = 1 and T_(n) = T_(n-1) +
T_(n-2) + T_(n-3).

It can be shown that 27 does not divide any terms of this sequence.
In fact, 27 is the first odd number with this property.

Find the 124^(th) odd number that does not divide any terms of the
above sequence.
-}

tribonacci = f 1 1 1
  where
    f a b c = a : f b c (a + b + c)

tribs_mod :: Int -> [Int]
tribs_mod n = f 1 1 1
  where
    f a b c = a : f b c ((a + b + c) `mod` n)

tribs_mod' :: Int -> [Int]
tribs_mod' n = 1 : f 1 1 (3 `mod` n)
  where
    f 1 1 1 = []
    f a b c = a : f b c ((a + b + c) `mod` n)

trib_cycle :: Int -> Int
trib_cycle n = f 1 1 1 (3 `mod` n)
  where
    f l 1 1 1 = l
    f l a b c = f (l+1) b c ((a + b + c) `mod` n)

no_multiples :: Int -> Bool
no_multiples n = 0 `notElem` (tribs_mod' n)
--[27,81,91,103,135,163,189,199,203,221,243,247,273,297,305,309,351,371,377,397,405,421,455,459,489,513,515,551,559,

--[27,81,91,103,135,163,189,199,203,221,243,247,273,297,305,309,351,371,377,397,405,421,455,459,489,513,515,551,559,567,597,609,621,637,663,675,721,729,741,757,779,783,815,819,837,883,891,915,927,945,991,995,999,1001,1015,1021,1053,1079,1087,1105,1107,1113,1123,1131,1133,1141,1161,1183,1189,1191,1199,1215,1235,1237,1263,1269,1323,1339,1351,1365,1377,1393,1421,1431,1467,1485,1521,1525,1539,1543,1545,1547,1567,1593,1609,1647,1651,1653,1677,1701,1729,1751,1753,1755,1769,1791,1793,1799,1807,1809,1827,1855,1863,1873,1883,1885,1911,1917,1919,1957,1971,1985,1989,2009]

no_multiple_list :: [Int]
no_multiple_list = f [3,5..]
  where
    f (n:ns)
      | no_multiples n = n : S.union (f (sieve n ns)) [n*k | k <- [3,5..]]
      | otherwise = f ns
    sieve n = filter (\k -> k `mod` n /= 0)

main :: IO String
main = return $ show $ no_multiple_list !! (124-1)
