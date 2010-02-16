module SquareRoot where
import Data.Array.Unboxed

--------------------------------------------------
-- Calculating integer square roots

-- square_root_aux n = (r, s) such that
-- n = r^2 + s, and 0 <= s < 2r+1
square_root_aux :: Integral a => a -> (a, a)
square_root_aux n
  | n <= 0    = (0, 0)
  | s' < smax = (2*r, s')
  | otherwise = (2*r+1, s'- smax)
  where
    (m, d) = divMod n 4
    (r, s) = square_root_aux m
    s' = 4*s + d
    smax = 4*r + 1

square_root :: Integral a => a -> a
square_root = fst . square_root_aux

--------------------------------------------------
-- Calculating integer cube roots

-- cube_root_aux n = (r, s) such that
-- n = r^3 + s, and 0 <= s < 3r^2+3r+1
cube_root_aux :: Integral a => a -> (a, a)
cube_root_aux n
  | n == -1   = (-1, 0)
  | n == 0    = (0, 0)
  | s' < smax = (2*r, s')
  | otherwise = (2*r+1, s'- smax)
  where
    (m, d) = divMod n 8
    (r, s) = cube_root_aux m
    s' = 8*s + d
    smax = 12*r^2 + 6*r + 1

cube_root :: Integral a => a -> a
cube_root = fst . cube_root_aux

--------------------------------------------------
-- Testing for perfect squares

qr_array :: Int -> UArray Int Bool
qr_array m = accumArray (const id) False (0, m-1)
  [ (n^2 `mod` m, True) | n <- [0 .. m `div` 2] ]

qr256 :: UArray Int Bool
qr256 = qr_array 256

qr255 :: UArray Int Bool
qr255 = qr_array 255

qr1001 :: UArray Int Bool
qr1001 = qr_array 1001

is_square :: Integral a => a -> Bool
is_square n =
  n >= 0 &&
  qr256 ! (fromIntegral (n `mod` 256)) &&
  qr255 ! (fromIntegral (n `mod` 255)) &&
  qr1001 ! (fromIntegral (n `mod` 1001)) &&
  snd (square_root_aux n) == 0
