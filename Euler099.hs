module Euler099 where
import EulerLib

------------------------------------------------------------------------------
-- 99. Which base/exponent pair in the file has the greatest numerical value?

max_exp xs = fst $ maximumOf snd pairs
  where
    score (x,y) = y * log x
    pairs = zip [1 ..] $ map score xs

base_expTxt :: IO [(Double,Double)]
base_expTxt = readFile "base_exp.txt" >>= return .
  map (\l -> read ("(" ++ l ++ ")")) . lines

main :: IO String
main = base_expTxt >>= return . show . max_exp
-- 709

