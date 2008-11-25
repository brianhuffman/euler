module Euler090 where
import Bits

------------------------------------------------------------------------------
-- 90. An unexpected way of using two cubes to make a square.
{-
Each of the six faces on a cube has a different digit (0 to 9) written on it;
the same is done to a second cube. By placing the two cubes side-by-side in
different positions we can form a variety of 2-digit numbers.

For example, the square number 64 could be formed:

In fact, by carefully choosing the digits on both cubes it is possible to
display all of the square numbers below one-hundred: 01, 04, 09, 16, 25, 36,
49, 64, and 81.

For example, one way this can be achieved is by placing {0, 5, 6, 7, 8, 9} on
one cube and {1, 2, 3, 4, 8, 9} on the other cube.

However, for this problem we shall allow the 6 or 9 to be turned upside-down
so that an arrangement like {0, 5, 6, 7, 8, 9} and {1, 2, 3, 4, 6, 7} allows
for all nine square numbers to be displayed; otherwise it would be impossible
to obtain 09.

In determining a distinct arrangement we are interested in the digits on each
cube, not the order.

{1, 2, 3, 4, 5, 6} is equivalent to {3, 6, 4, 1, 2, 5}
{1, 2, 3, 4, 5, 6} is distinct from {1, 2, 3, 4, 5, 9}

But because we are allowing 6 and 9 to be reversed, the two distinct sets in
the last example both represent the extended set {1, 2, 3, 4, 5, 6, 9} for the
purpose of forming 2-digit numbers.

How many distinct arrangements of the two cubes allow for all of the square
numbers to be displayed?
-}

-- Key represents a set of digits
-- bit n represents presence of digit n
type Key = Int

-- keys_len n m = list of keys with n of m bits set.
keys_len 0 _ = [0]
keys_len n 0 = []
keys_len n m =
  keys_len n (m-1) ++ map (flip setBit (m-1)) (keys_len (n-1) (m-1))

str :: Key -> String
str = s '0'
  where
     s c 0 = ""
     s c k = let cs = s (succ c) (k`div`2)
             in if odd k then c : cs else cs

arrangements :: [(String, String)]
arrangements =
  [ (str x, str y) |
    x <- keys_len 6 10,
    y <- keys_len 6 10,
    x <= y,
    let x' = extend x,
    let y' = extend y,
    all (test x' y') squares ]
  where
    extend k
      | testBit k 6 = k .|. bit 9
      | testBit k 9 = k .|. bit 6
      | otherwise   = k 
    test x y (a, b) =
      (testBit x a && testBit y b) || (testBit y a && testBit x b)
    squares = [(0,1),(0,4),(0,9),(1,6),(2,5),(3,6),(4,9),(6,4),(8,1)]

main :: IO String
main = return $ show $ length arrangements
-- 1217
