module Euler081 where
import EulerLib
import Array

------------------------------------------------------------------------------
-- 81. Find the minimal path sum from the top left to the bottom right by moving right and down.

type Matrix = Array (Int, Int) Int

test :: Matrix
test = listArray ((0,0),(4,4))
 [131, 673, 234, 103,  18,
  201,  96, 342, 965, 150,
  630, 803, 746, 422, 111,
  537, 699, 497, 121, 956,
  805, 732, 524,  37, 331]

minimal_path_sum :: Matrix -> Int
minimal_path_sum e = a!(r0, c0)
  where
    bnds@((r0,c0),(r1,c1)) = bounds e
    a = funArray bnds f
    -- f (r, c) is minimal sum from (r, c) to (r1, c1) 
    f (r, c)
      | (r, c) == (r1, c1) = e!(r, c)
      | r == r1 && c == c1 = e!(r, c) 
      | r == r1            = e!(r, c) + a!(r, c+1)
      | c == c1            = e!(r, c) + a!(r+1, c)
      | otherwise          = e!(r, c) + min (a!(r+1, c)) (a!(r, c+1))

matrixTxt :: IO Matrix
matrixTxt = do 
  s <- readFile "matrix.txt"
  let ls = lines s
  let es = concatMap (\s -> read ("[" ++ s ++ "]")) ls
  return (listArray ((0,0),(79,79)) es)

main :: IO String
main = matrixTxt >>= return . show . minimal_path_sum
-- 427337
