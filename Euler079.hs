module Euler079 where
import EulerLib
import List

------------------------------------------------------------------------------
-- 79. By analysing a user's login attempts, can you determine the secret numeric passcode?
{-
A common security method used for online banking is to ask the user for three
random characters from a passcode. For example, if the passcode was 531278,
they may asked for the 2nd, 3rd, and 5th characters; the expected reply would
be: 317.

The text file, keylog.txt, contains fifty successful login attempts.

Given that the three characters are always asked for in order, analyse the file
so as to determine the shortest possible secret passcode of unknown length.
-}

-- contents of keylog.txt
keylog :: [String]
keylog =
 ["319","680","180","690","129","620","762","689","762","318",
  "368","710","720","710","629","168","160","689","716","731",
  "736","729","316","729","729","710","769","290","719","680",
  "318","389","162","289","162","718","729","319","790","680",
  "890","362","319","760","316","729","380","319","728","716"]

-- keylog' = nub keylog
keylog' :: [String]
keylog' =
 ["319","680","180","690","129","620","762","689","318","368","710",
  "720","629","168","160","716","731","736","729","316","769","290",
  "719","389","162","289","718","790","890","362","760","380","728"]
-- contains digits 01236789

is_subseq :: String -> String -> Bool
is_subseq [] ys = True
is_subseq (x:xs) [] = False
is_subseq (x:xs) (y:ys) =
  (x == y && is_subseq xs ys) || is_subseq (x:xs) ys

passcodes :: Int -> [String] -> [String]
passcodes _ [] = return []
passcodes 0 _ = []
passcodes n subs = do
  -- choose character appearing at the head of the most strings
  c <- reverse $ map head $ sortOf length $ group $ sort $ map head subs
  let subs1 = map (\s -> if c == head s then tail s else s) subs
  let subs2 = nub $ filter (not . null) subs1
  cs <- passcodes (n-1) subs2
  return (c:cs)

-- minimum length is the number of unique symbols in keylog

prob79 :: [String] -> [String]
prob79 keys = concat [ passcodes l keys | l <- [lmin ..] ]
  where lmin = length $ nub $ concat keys

main :: IO String
main = return $ head $ prob79 keylog'
-- "73162890"
