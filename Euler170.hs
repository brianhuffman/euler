module Euler170 where
import Permutation
import Data.List

------------------------------------------------------------------------------
-- 170. Find the largest 0 to 9 pandigital that can be formed by concatenating products.
{-
one of the products must carry to an extra digit.
-}

delete1 x [] = []
delete1 x (y:ys)
  | x == y = [ys]
  | otherwise = [ y:ys' | ys' <- delete1 x ys ]

prob170a k c [] [] = if c == 0 then [[]] else []
prob170a k c [] ys =
  [ zs |
    let (c',y) = divMod c 10,
    ys' <- delete1 y ys,
    zs <- prob170a k c' [] ys' ]
prob170a k c xs ys =
  [ x:zs |
    (x,xs') <- remove1 xs,
    let (c',y) = divMod (k*x + c) 10,
    ys' <- delete1 y ys,
    zs <- prob170a k c' xs' ys' ]

value = foldr (\d x -> 10*x + d) 0

prob170 =
  [ (n*k, k, n) |
    let xs0 = [0 .. 9],
    (k,xs) <- remove1 xs0,
    ds <- prob170a k 0 (0:xs) [0 .. 9],
    let n = value ds ]
-- 9 * 187345026 = 9786105234
-- 9 * 1,87345026 = 9,786105234
-- 9 * 1087345,26 = 9786105,234

prob170' =
  [ (n*k, k, n) |
    let xs0 = [0 .. 9],
    (k1,xs1) <- remove1 xs0,
    (k2,xs2) <- remove1 xs1,
    let k = 10*k1 + k2,
    ds <- prob170a k 0 (0:xs2) [0 .. 9],
    let n = value ds ]
-- 27 * 365080149 = 9857164023
-- 27 * 36508,149 = 985716,4023
