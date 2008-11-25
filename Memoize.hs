-- Unboxed memo tables.

{-# LANGUAGE FlexibleContexts #-}

module Memoize ( memoize, memoizeU ) where

import GHC.Arr ( unsafeIndex )
import Data.Array.Base
import Data.Array.IO
import System.IO.Unsafe	( unsafePerformIO )

memoizeU :: (Ix i, MArray IOUArray e IO)
        => (i, i) -> (i -> e) -> (i -> e)
memoizeU (l,u) f =
  unsafePerformIO $ do
    a <- newUnboxedMemo (l,u)
    b <- newBoolTable (l,u)
    let read x
          | inRange (l,u) x =
              unsafePerformIO $ do
                let i = unsafeIndex (l,u) x
                e <- unsafeRead b i
                if e then unsafeRead a i else do
                let y = f x
                unsafeWrite a i y
                unsafeWrite b i True
                return y
          | otherwise = f x
    return read

memoize :: (Ix i) => (i, i) -> (i -> e) -> (i -> e)
memoize (l,u) f =
  unsafePerformIO $ do
    a <- newBoxedMemo (l,u)
    b <- newBoolTable (l,u)
    let read x
          | inRange (l,u) x =
              unsafePerformIO $ do
                let i = unsafeIndex (l,u) x
                e <- unsafeRead b i
                if e then unsafeRead a i else do
                let y = f x
                unsafeWrite a i y
                unsafeWrite b i True
                return y
          | otherwise = f x
    return read

newUnboxedMemo :: (Ix i, MArray IOUArray e IO) => (i, i) -> IO (IOUArray i e)
newUnboxedMemo (l,u) = unsafeNewArray_ (l,u)

newBoxedMemo :: (Ix i) => (i, i) -> IO (IOArray i e)
newBoxedMemo (l,u) = unsafeNewArray_ (l,u)

newBoolTable :: (Ix i) => (i, i) -> IO (IOUArray i Bool)
newBoolTable (l,u) = newArray (l,u) False


{-
memoize :: (Ix i) => (i, i) -> (i -> e) -> i -> e
memoize ij f = g
  where
    a = listArray ij (map f (range ij))
    g x = if inRange ij x then a ! x else f x
-}
