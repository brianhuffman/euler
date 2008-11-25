module Euler109 where

------------------------------------------------------------------------------
-- 109. How many distinct ways can a player checkout in the game of darts with a score of less than 100?

checkouts :: Int -> [[(Int, Int)]]
checkouts m =
  [ [z] | z <- doubles, score z < m ] ++
  [ [y,z] | z <- doubles, y <- targets, score y + score z < m ] ++
  [ [x,y,z] | z <- doubles, y <- targets, x <- targets,
    x <= y, score x + score y + score z < m ]
  where
    singles = [ (1,n) | n <- 25 : [1 .. 20] ]
    doubles = [ (2,n) | n <- 25 : [1 .. 20] ]
    triples = [ (3,n) | n <- [1 .. 20] ]
    targets = singles ++ doubles ++ triples
    score (m,n) = m * n

main :: IO String
main = return $ show $ length $ checkouts 100
-- 38182

