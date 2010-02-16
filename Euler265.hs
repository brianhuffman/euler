module Euler265 where
import Data.Bits
import Data.Word

{---------------------------------------------------------------------
Problem 265
Binary Circles

21 November 2009

2^(N) binary digits can be placed in a circle so that all the N-digit
clockwise subsequences are distinct.

For N=3, two such circular arrangements are possible, ignoring
rotations:

    0  0           0  0
  1      0       1      0
  1      1       0      1
    1  0           1  1

For the first arrangement, the 3-digit subsequences, in clockwise
order, are: 000, 001, 010, 101, 011, 111, 110 and 100.

Each circular arrangement can be encoded as a number by concatenating
the binary digits starting with the subsequence of all zeros as the
most significant bits and proceeding clockwise. The two arrangements
for N=3 are thus represented as 23 and 29:

00010111 _(2) = 23
00011101 _(2) = 29

Calling S(N) the sum of the unique numeric representations, we can see
that S(3) = 23 + 29 = 52.

Find S(5).

---------------------------------------------------------------------}

type W = Word32

size :: Int
size = 5

mask :: W
mask = bit size - 1

allbits :: W
allbits = bit (2^size) - 1

circles :: W -> W -> [W]
circles r n
  | r == allbits = if n .&. mask == 0 then [n `shiftR` size] else []
  | otherwise =
    [ ns |
      let n0 = n `shiftL` 1,
      let n1 = n0 .|. 1,
      n' <- [n0, n1],
      let b = fromIntegral (n' .&. mask),
      not (testBit r b),
      let r' = r .|. bit b,
      ns <- circles r' n'
    ]

main :: IO String
main = return $ show $ sum $ map toInteger $ circles 0 0

answer :: String
answer = "209110240768"

-- length 1: 1 solution
-- length 2: 1 solution
-- length 3: 2 solutions
-- length 4: 16 solutions
-- length 5: 2048 solutions
