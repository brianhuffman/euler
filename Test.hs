{-# LANGUAGE FlexibleContexts #-}

module Test where
import GHC.Arr ( unsafeIndex )
import Data.Array.Base
import Data.Array.IO
import System.IO.Unsafe	( unsafePerformIO )

data UMemo =
  UMemo (Int -> Bool) (Int, Int) (IOUArray Int Bool) (IOUArray Int Bool)

readUMemo :: UMemo -> Int -> Bool
readUMemo (UMemo f (l,u) a b) x
  | inRange (l,u) x =
      unsafePerformIO $ do
        let i = unsafeIndex (l,u) x
        e <- unsafeRead b i
        if e then do y <- unsafeRead a i
                     putStrLn ("<Reading " ++ show (i,y) ++ ">")
                     return y
             else do
        let y = f x
        putStrLn ("<Writing " ++ show i ++ ">")
        unsafeWrite a i y
        putStrLn ("<Working " ++ show (i,y) ++ ">")
        unsafeWrite b i True
        putStrLn ("<Done " ++ show (i,y) ++ ">")
        return y
  | otherwise = f x

newUMemo :: (Int, Int) -> (Int -> Bool) -> UMemo
newUMemo (l,u) f =
  unsafePerformIO $ do
    a <- newUnboxedMemo (l,u)
    b <- newBoolTable (l,u)
    return (UMemo f (l,u) a b)

newUnboxedMemo :: (Int, Int) -> IO (IOUArray Int Bool)
newUnboxedMemo (l,u) = unsafeNewArray_ (l,u)

newBoolTable :: (Int, Int) -> IO (IOUArray Int Bool)
newBoolTable (l,u) = newArray (l,u) False

mkMemo :: () -> UMemo
mkMemo () = m
  where
    m = newUMemo (0, 1) g
    g 0 = True
    g 1 = readUMemo m 0

test :: [Int] -> [Bool]
test xs = [ readUMemo a x | x <- xs ]
  where a = mkMemo ()

-- test [1,0] is nondeterministic

------------------------------------------------------------

test' :: IO [Bool]
test' =
  do
    a <- newUnboxedMemo (0,1)
    b <- newBoolTable (0,1)
    let m = UMemo' f a b
        f 0 = True
        f 1 = unsafePerformIO (readUMemo' m 0)
    x1 <- readUMemo' m 1
    x0 <- readUMemo' m 0
    return [x1, x0]

data UMemo' =
  UMemo' (Int -> Bool) (IOUArray Int Bool) (IOUArray Int Bool)

readUMemo' :: UMemo' -> Int -> IO Bool
readUMemo' (UMemo' f a b) i = do
    e <- unsafeRead b i
    if e then do y <- unsafeRead a i
                 putStrLn ("<Reading " ++ show (i,y) ++ ">")
                 return y
         else do
    let y = f i
    putStrLn ("<Calculating " ++ show i ++ ">")
    x0 <- unsafeRead a 0
    x1 <- unsafeRead a 1
    putStrLn ("<Start Contents " ++ show i ++ " " ++ show [x0,x1] ++ ">")
    -- here y is not yet evaluated!
    unsafeWrite a i y
    putStrLn ("<Writing " ++ show (i,y) ++ ">")
    x0 <- unsafeRead a 0
    x1 <- unsafeRead a 1
    putStrLn ("<Final Contents " ++ show i ++ " " ++ show [x0,x1] ++ ">")
    unsafeWrite b i True
    putStrLn ("<Marking " ++ show (i,y) ++ ">")
    return y


