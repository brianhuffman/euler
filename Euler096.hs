module Euler096 where
import EulerLib
import qualified SortedList as S
import Data.Array
import Data.Maybe
import Control.Monad
import Data.List

------------------------------------------------------------------------------
-- 96. Devise an algorithm for solving Su Doku puzzles.
{-
Su Doku (Japanese meaning number place) is the name given to a popular puzzle concept. Its origin is unclear, but credit must be attributed to Leonhard Euler who invented a similar, and much more difficult, puzzle idea called Latin Squares. The objective of Su Doku puzzles, however, is to replace the blanks (or zeros) in a 9 by 9 grid in such that each row, column, and 3 by 3 box contains each of the digits 1 to 9. Below is an example of a typical starting puzzle grid and its solution grid.
0 0 3   0 2 0   6 0 0
9 0 0   3 0 5   0 0 1
0 0 1   8 0 6   4 0 0

0 0 8   1 0 2   9 0 0
7 0 0   0 0 0   0 0 8
0 0 6   7 0 8   2 0 0

0 0 2   6 0 9   5 0 0
8 0 0   2 0 3   0 0 9
0 0 5   0 1 0   3 0 0



4 8 3   9 2 1   6 5 7
9 6 7   3 4 5   8 2 1
2 5 1   8 7 6   4 9 3

5 4 8   1 3 2   9 7 6
7 2 9   5 6 4   1 3 8
1 3 6   7 9 8   2 4 5

3 7 2   6 8 9   5 1 4
8 1 4   2 5 3   7 6 9
6 9 5   4 1 7   3 8 2

A well constructed Su Doku puzzle has a unique solution and can be solved by
logic, although it may be necessary to employ "guess and test" methods in order
to eliminate options (there is much contested opinion over this). The complexity
of the search determines the difficulty of the puzzle; the example above is
considered easy because it can be solved by straight forward direct deduction.

The 6K text file, sudoku.txt, contains fifty different Su Doku puzzles ranging
in difficulty, but all with unique solutions (the first puzzle in the file is
the example above).

By solving all fifty puzzles find the sum of the 3-digit numbers found in the
top left corner of each solution grid; for example, 483 is the 3-digit number
found in the top left corner of the solution grid above.
-}

-- coordinates are (major row, minor row, major col, minor col)
type Sudoku = Array (Int, Int, Int, Int) Char

sudokuTxt :: IO [Sudoku]
sudokuTxt = readFile "sudoku.txt" >>=
  return . map (listArray ((0,0,0,0),(2,2,2,2))) .
  map concat . map (drop 1) . chunk 10 . lines .
  filter (/='\r')

solve_sudoku :: Sudoku -> [Sudoku]
solve_sudoku sdk
  | null zeroes = [sdk]
  | otherwise = msum [ solve_sudoku (sdk // [(p', n)]) | n <- ns ]
  where
    zeroes = [ p | (p, n) <- assocs sdk, n == '0' ]
    (p', ns) = minimumOf (length . snd) [ (p, all_rem ! p) | p <- zeroes ]
    ls = [0, 1, 2]
    arr2 = listArray ((0,0),(2,2))
    box (r,y,c,x) = (r,c)
    row (r,y,c,x) = (r,y)
    col (r,y,c,x) = (c,x)
    box_ps = [ [(r,y,c,x) | y <- ls, x <- ls ] | r <- ls, c <- ls ]
    row_ps = [ [(r,y,c,x) | c <- ls, x <- ls ] | r <- ls, y <- ls ]
    col_ps = [ [(r,y,c,x) | r <- ls, y <- ls ] | c <- ls, x <- ls ]
    remaining ps = "123456789" \\ (map (sdk!) ps)
    box_rem = arr2 (map remaining box_ps)
    row_rem = arr2 (map remaining row_ps)
    col_rem = arr2 (map remaining col_ps)
    all_rem = funArray ((0,0,0,0),(2,2,2,2))
      (\p -> foldl1 S.intersect [box_rem!box p, row_rem!row p, col_rem!col p])

show_sudoku :: Sudoku -> String
show_sudoku = elems

main :: IO String
main = sudokuTxt >>=
  return . show . sum .
  map (read . take 3 . show_sudoku) .
  concatMap solve_sudoku
-- 24702

