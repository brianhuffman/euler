module Polynomial
  (
    Poly (..),
    pX,
    eval,
    coeff
  )
where

newtype Poly a = Poly [a]
  deriving (Eq, Show)

instance Num a => Num (Poly a) where
  Poly xs + Poly ys = Poly (addP xs ys)
  Poly xs - Poly ys = Poly (subP xs ys)
  Poly xs * Poly ys = Poly (mulP xs ys)
  negate (Poly xs) = Poly (negP xs)
  signum (Poly xs) = undefined
  abs (Poly xs) = undefined
  fromInteger n = Poly (consP (fromInteger n) [])

instance (Num a, Ord a) => Ord (Poly a) where
  compare (Poly xs) (Poly ys) = compareP xs ys

instance Num a => Enum (Poly a) where
  succ xs = xs + 1
  toEnum = undefined
  fromEnum = undefined

instance (Num a, Ord a) => Real (Poly a) where
  toRational (Poly xs) = error "toRational: Poly"

instance (Ord a, Fractional a) => Integral (Poly a) where
  divMod (Poly xs) (Poly ys) = (Poly qs, Poly rs)
    where (qs, rs) = divModP xs ys
  quotRem = divMod
  toInteger = error "toInteger: Poly"

consP :: Num a => a -> [a] -> [a]
consP 0 [] = []
consP x xs = x : xs

addP :: Num a => [a] -> [a] -> [a]
addP (x : xs) (y : ys) = consP (x + y) (addP xs ys)
addP [] ys = ys
addP xs [] = xs

scaleP :: Num a => a -> [a] -> [a]
scaleP x ys = map (x*) ys

mulP :: Num a => [a] -> [a] -> [a]
mulP [] ys = []
mulP (x : xs) ys = addP (scaleP x ys) (consP 0 (mulP xs ys))

subP :: Num a => [a] -> [a] -> [a]
subP (x : xs) (y : ys) = consP (x - y) (subP xs ys)
subP [] ys = negP ys
subP xs [] = xs

negP :: Num a => [a] -> [a]
negP = map negate

compareP :: (Num a, Ord a) => [a] -> [a] -> Ordering
compareP [] [] = EQ
compareP [] ys = compareP [0] ys
compareP xs [] = compareP xs [0]
compareP (x : xs) (y : ys) =
  case compare xs ys of
    EQ -> compare x y
    LT -> LT
    GT -> GT

divModP :: Fractional a => [a] -> [a] -> ([a], [a])
divModP xs [] = ([], xs)
divModP [] ys = ([], [])
divModP (x : xs) ys
  | b == 0 = (consP 0 qs, consP x rs)
  | b /= 0 = (consP b qs, subP rs' (scaleP b ys))
  where
    (qs, rs) = divModP xs ys
    rs' = consP x rs
    deg = degreeP ys
    b = coeffP rs' deg / coeffP ys deg

------------------------------------------------------

pX :: Num a => Poly a
pX = Poly [0, 1]

eval :: Num a => Poly a -> a -> a
eval (Poly xs) = evalP xs

evalP [] y = 0
evalP (x : xs) y = x + y * evalP xs y

coeff :: Num a => Poly a -> Int -> a
coeff (Poly xs) = coeffP xs

coeffP :: Num a => [a] -> Int -> a
coeffP [] n = 0
coeffP (x : xs) n
  | n > 0     = coeffP xs (n-1)
  | otherwise = x

degree :: Poly a -> Int
degree (Poly xs) = degreeP xs

degreeP :: [a] -> Int
degreeP [] = 0
degreeP (_ : xs) = length xs
