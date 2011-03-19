module Euler287 where

{-	

Problem 287
Quadtree encoding (a simple compression algorithm)
10 April 2010

The quadtree encoding allows us to describe a 2^N × 2^N black and
white image as a sequence of bits (0 and 1). Those sequences are to be
read from left to right like this:

    * the first bit deals with the complete 2^N × 2^N region;

    * "0" denotes a split: the current 2^n × 2^n region is divided
      into 4 sub-regions of dimension 2^(n-1) × 2^(n-1), the next bits
      contains the description of the top left, top right, bottom left
      and bottom right sub-regions - in that order;

    * "10" indicates that the current region contains only black pixels;

    * "11" indicates that the current region contains only white pixels.

Consider the following 4×4 image (colored marks denote places where a
split can occur):

     +---+---+---+---+
  y=3|###|###|###|   |
     +---b---+---c---+
  y=2|###|###|   |###|
     +---+---a---+---+
  y=1|   |   |###|###|
     +---d---+---e---+
  y=0|   |   |###|###|
     +---+---+---+---+
      x=0 x=1 x=2 x=3

This image can be described by several sequences, for example:
"001010101001011111011010101010", of length 30, or "0100101111101110",
of length 16, which is the minimal sequence for this image.

For a positive integer N, define D_N as the 2^N × 2^N image with the
following coloring scheme:

    * the pixel with coordinates x = 0, y = 0 corresponds to the
      bottom left pixel,
    * if (x - 2^(N-1))^2 + (y - 2^(N-1))^2 ≤ 2^(2N-2) then the pixel
      is black,
    * otherwise the pixel is white.

What is the length of the minimal sequence describing D_24 ?
	
-}

image n = reverse
  [ [ if b then '#' else '-' |
      x <- [0..(2^n)-1],
      let b = (x-2^(n-1))^2 + (y-2^(n-1))^2 <= 2^(2*n-2) ] |
    y <- [0..(2^n)-1] ]

-- count number of splits in encoding
-- assume circle centered at (0, 0)
-- assume 0 <= xmin, 0 <= ymin
-- assume size is a power of 2
circle_splits r2 1 _ = 0
circle_splits r2 size (x0,y0)
  | is_black (x0,y0) == is_black (x3,y3) = 0
  | otherwise = 1 + sum [ circle_splits r2 half xy | xy <- xys ]
  where
    half = size `div` 2
    x1 = x0 + half - 1
    y1 = y0 + half - 1
    x2 = x0 + half
    y2 = y0 + half
    x3 = x0 + size - 1
    y3 = y0 + size - 1
    is_black (x, y) = x^2 + y^2 <= r2
    xys = [(x0,y0),(x0,y2),(x2,y0),(x2,y2)]

total_splits n = 1 + sum [ circle_splits r2 size xy | xy <- xys ]
  where
    size = 2^(n-1)
    r2 = 2^(2*n-2)
    xys = [(0,0),(0,1),(1,0),(1,1)]

-- k splits, 3k+1 blocks. Cost = k + 2(3k+1) = 7k+2.
total_cost n = 7 * total_splits n + 2

{-
total_splits 3 = 12
total_splits 4 = 30
total_splits 5 = 71
total_splits 6 = 150
total_splits 7 = 320
total_splits 8 = 650
total_splits 9 = 1340
total_splits 10 = 2684
total_splits 11 = 5351
total_splits 12 = 10808
total_splits 13 = 21673
total_splits 14 = 43484
total_splits 15 = 87116
total_splits 16 = 174470
total_splits 17 = 349425
total_splits 18 = 698650
total_splits 19 = 1397595
total_splits 20 = 2795338
total_splits 21 = 5591785
total_splits 22 = 11182240
total_splits 23 = 22368575
total_splits 24 = 44733642
-}

main :: IO String
main = return $ show $ total_cost 24

answer :: String
answer = "313135496"
