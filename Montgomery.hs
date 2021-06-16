module Montgomery where
import Primes
import Data.Word
import Data.Bits
{-
step :: Integer -> Integer -> Integer
step x y = if w >= n then w - n else w
  where
    r = 2^32
    n = 93992931
    k = r - invMod n r

    z = x * y
    z0 = z `mod` r
    u = (k * z0) `mod` r
    v = u * n
    w' = z + v
    w = w' `div` r

prep :: Int -> Integer
prep x = step (toInteger x) (2^64 `mod` 93992931)

post :: Integer -> Int
post x = fromInteger (step x 1)

n :: Int
n = 93992931

k :: Int
k = 2185443893
-- n divides 0x58c83ffffffff

x, y, z :: Int
x = 1234567
y = 2345678
z = 68440247

x' = 75338005
y' = 76050473
z' = 53510153

timesModInt :: Int -> Int -> Int -> Int
timesModInt a b n = post (step' n32 k32 (prep a) (prep b))
  where
    n32 = fromIntegral n :: Word32
    n64 = fromIntegral n :: Word64
    r64 = 0x100000000 :: Word64
    r1 = r64 `mod` n64
    r2 = r1 * r1 `mod` n64
    k32 = - fromIntegral (invMod n64 r64) :: Word32
    prep :: Int -> Word32
    prep x = step' n32 k32 (fromIntegral x) (fromIntegral r2)
    post :: Word32 -> Int
    post x32 = fromIntegral (step' n32 k32 x32 1)

step' :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
step' n32 k32 x32 y32 = if w32 >= n32 then w32 - n32 else w32
  where
    x64 = fromIntegral x32 :: Word64
    y64 = fromIntegral y32 :: Word64
    n64 = fromIntegral n32 :: Word64
    v64 = fromIntegral (x32 * y32 * k32) * n64
    w64 = x64 * y64 + v64
    w32 = fromIntegral (shiftR w64 32)
-}

expModInt :: Int -> Int -> Int -> Int
expModInt a 0 n = a `mod` n
expModInt a b n = post (exp (prep a) b)
  where
    n32 = fromIntegral n :: Word32
    n64 = fromIntegral n :: Word64
    r64 = 0x100000000 :: Word64
    r1 = r64 `mod` n64
    r2 = r1 * r1 `mod` n64
    k32 = - fromIntegral (invMod n64 r64) :: Word32
    prep :: Int -> Word32
    prep x = step (fromIntegral x) (fromIntegral r2)
    post :: Word32 -> Int
    post x32 = fromIntegral (step x32 1)
    step :: Word32 -> Word32 -> Word32
    step x32 y32 = if w32 >= n32 then w32 - n32 else w32
      where
        x64 = fromIntegral x32 :: Word64
        y64 = fromIntegral y32 :: Word64
        v64 = fromIntegral (x32 * y32 * k32) * n64
        w64 = x64 * y64 + v64
        w32 = fromIntegral (shiftR w64 32)
    exp :: Word32 -> Int -> Word32
    exp x32 1 = x32
    exp x32 b = seq y32 (if even b then y32 else step x32 y32)
      where y32 = exp (step x32 x32) (b `div` 2)

{-
expMod a b n 
  | b == 0 = 1 `mod` n
  | b == 1 = a `mod` n
  | otherwise = seq x (if even b then x else (a * x) `mod` n)
      where x = expMod ((a * a) `mod` n) (b `div` 2) n
-}

showHex 0 = "0"
showHex n = reverse (f n)
  where
    s = "0123456789ABCDEF"
    f 0 = ""
    f n = (s!!fromIntegral r) : f q
      where (q, r) = divMod n 16

{-
Using Montgomery reduction to make an efficient expMod
------------------------------------------------------

let R = 2^32
odd modulus n < R

define k = -n^-1 (mod R)
such that k*n = -1 (mod R)

a[i] = a^i * R (mod n)
a[1] = a*R (mod n)
a[i+j] = a[i] * a[j] * R^-1 (mod n)

To compute a[i+j]:

compute a[i]*a[j] (result is 64-bit number)


a' = a[i] * a[j]    (a' :: Int64)
u = (k * a') mod R  (u :: Int32)
v = u * n           (v :: Int64)
w = a' + v          (w :: Int64)
a[i+j] = w / R

-}