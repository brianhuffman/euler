-- Memo tables.

{-# LANGUAGE FlexibleContexts #-}

module UnboxedMemo ( UMemo, newUMemo, readUMemo ) where

import GHC.Arr ( unsafeIndex )
import Data.Array.Base
import Data.Array.IO
import System.IO.Unsafe	( unsafePerformIO )

data UMemo i e =
  UMemo (i -> e) !(i, i) !(IOUArray i e) !(IOUArray i Bool)

readUMemo ::(Ix i, MArray IOUArray e IO, Show i, Show e) => UMemo i e -> i -> e
readUMemo (UMemo f (l,u) a b) x
  | inRange (l,u) x =
      unsafePerformIO $ do
        let i = unsafeIndex (l,u) x
        e <- unsafeRead b i
        if e then unsafeRead a i else do
        let y = f x
        unsafeWrite a i $! y
        unsafeWrite b i True
        return y
  | otherwise = f x

newUMemo ::(Ix i, MArray IOUArray e IO) => (i, i) -> (i -> e) -> UMemo i e
newUMemo (l,u) f =
  unsafePerformIO $ do
    a <- newUnboxedMemo (l,u)
    b <- newBoolTable (l,u)
    return (UMemo f (l,u) a b)

newUnboxedMemo :: (Ix i, MArray IOUArray e IO) => (i, i) -> IO (IOUArray i e)
newUnboxedMemo (l,u) = unsafeNewArray_ (l,u)

newBoolTable :: (Ix i) => (i, i) -> IO (IOUArray i Bool)
newBoolTable (l,u) = newArray (l,u) False
