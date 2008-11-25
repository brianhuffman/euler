module Euler045 where
import qualified SortedList as S

------------------------------------------------------------------------------
-- 45. After 40755, what is the next triangle number that is also pentagonal and hexagonal?

polygon_seq :: Integer -> [Integer]
polygon_seq n = scanl1 (+) [1, n-1 ..]

--triangle_seq = scanl1 (+) [1,2 ..]
--pentagon_seq = scanl1 (+) [1,4 ..]
--hexagon_seq = scanl1 (+) [1,5 ..]

tri_pent_hex :: [Integer]
tri_pent_hex = foldl1 S.intersect (map polygon_seq [3,5,6])
-- [1,40755,1533776805,57722156241751

main :: IO String
main = return $ show $ head $ dropWhile (<= 40755) tri_pent_hex

