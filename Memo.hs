-- Memo tables.

{-# LANGUAGE FlexibleContexts #-}

module Memo ( Memo, newMemo, readMemo ) where

import GHC.Arr ( unsafeIndex )
import Data.Array.Base
import Data.Array.IO
import System.IO.Unsafe	( unsafePerformIO )

data Memo i e =
  Memo (i -> e) (i, i) (IOArray i e) (IOUArray i Bool)

readMemo :: (Ix i) => Memo i e -> i -> e
readMemo (Memo f (l,u) a b) x
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

newMemo :: (Ix i) => (i, i) -> (i -> e) -> Memo i e
newMemo (l,u) f =
  unsafePerformIO $ do
    a <- newBoxedMemo (l,u)
    b <- newBoolTable (l,u)
    return (Memo f (l,u) a b)

newBoxedMemo :: (Ix i) => (i, i) -> IO (IOArray i e)
newBoxedMemo (l,u) = unsafeNewArray_ (l,u)

newBoolTable :: (Ix i) => (i, i) -> IO (IOUArray i Bool)
newBoolTable (l,u) = newArray (l,u) False
