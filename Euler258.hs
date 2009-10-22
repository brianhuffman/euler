module Euler258 where
import Data.Array.Unboxed

{-
Problem 258
03 October 2009

A sequence is defined as:

    * g_(k) = 1, for 0 ≤ k ≤ 1999
    * g_(k) = g_(k-2000) + g_(k-1999), for k ≥ 2000. 

Find g_(k) mod 20092010 for k = 10^(18). 

-}

-- 20092010 = 8590 * 2339

type Z = Int

add :: Z -> Z -> Z
add x y = if z > 20092010 then z - 20092010 else z
  where z = x + y

mul :: Z -> Z -> Z
mul x y = fromInteger ((toInteger x * toInteger y) `mod` 20092010)

g :: [Z]
g = replicate 2000 1 ++ zipWith add g (tail g)

type Poly = UArray Int Z
-- zero-indexed

makePoly :: Int -> [(Int, Z)] -> Poly
makePoly n xs = seq (a!0) a
  where
    a = accumArray add 0 (0, n-1) upds
    upds =
      [ (i', x) |
        (i, x) <- xs,
        i' <- if i < n then [i] else [i-n, i-n+1]
      ]

shiftPoly :: Poly -> Poly
shiftPoly p = makePoly n [ (i+1, x) | (i, x) <- assocs p ]
  where n = rangeSize (bounds p)

squarePoly :: Poly -> Poly
squarePoly p = makePoly n upds
  where
    n = rangeSize (bounds p)
    upds =
      [ (i+j, xy') |
        i <- [0 .. n-1],
        let x = p!i, x /= 0,
        j <- [i .. n-1],
        let y = p!j, y /= 0,
        let xy = mul x y,
        let xy' = if i < j then add xy xy else xy
      ]

startPoly :: Int -> Poly
startPoly n = makePoly n [(0, 1)]

powerPoly :: Int -> Integer -> Poly
powerPoly n = f
  where
    f 0 = startPoly n
    f k | even k    = squarePoly (f (k`div`2))
        | otherwise = shiftPoly (squarePoly (f (k`div`2)))

gPoly :: Integer -> Z
gPoly i = foldl add 0 (elems (powerPoly 2000 i))

main :: IO String
main = return $ show $ gPoly (10^18)

answer :: String
answer = "12747994"
