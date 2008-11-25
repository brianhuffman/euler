module Euler082 where
import EulerLib
import Array

------------------------------------------------------------------------------
-- 82. Find the minimal path sum from the left column to the right column.

type Matrix = Array (Int, Int) Int

test :: Matrix
test = listArray ((0,0),(4,4))
 [131, 673, 234, 103,  18,
  201,  96, 342, 965, 150,
  630, 803, 746, 422, 111,
  537, 699, 497, 121, 956,
  805, 732, 524,  37, 331]

minimal_path_sum :: Matrix -> Int
minimal_path_sum e = minimum [ a1!(r, c0) | r <- [r0 .. r1] ]
  where
    bnds@((r0,c0),(r1,c1)) = bounds e
    a1 = funArray bnds f1
    a2 = funArray bnds f2
    a3 = funArray bnds f3
    -- minimal path sum from (r, c) to c1.
    f1 (r, c)
      | c == c1 = e!(r, c)
      | otherwise = min (a2!(r, c)) (a3!(r, c))
    -- minimal path sum from (r, c) to c1, initially moving up.
    f2 (r, c)
      | c == c1 = e!(r, c)
      | r == r0 = e!(r, c) + a1!(r, c+1)
      | otherwise = e!(r, c) + min (a2!(r-1, c)) (a1!(r, c+1))
    -- minimal path sum from (r, c) to c1, initially moving down.
    f3 (r, c)
      | c == c1 = e!(r, c)
      | r == r1 = e!(r, c) + a1!(r, c+1)
      | otherwise = e!(r, c) + min (a3!(r+1, c)) (a1!(r, c+1))

matrixTxt :: IO Matrix
matrixTxt = do 
  s <- readFile "matrix.txt"
  let ls = lines s
  let es = concatMap (\s -> read ("[" ++ s ++ "]")) ls
  return (listArray ((0,0),(79,79)) es)

main :: IO String
main = matrixTxt >>= return . show . minimal_path_sum
-- 260324
