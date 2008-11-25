module Euler205 where

{-
Peter has nine four-sided (pyramidal) dice,
each with faces numbered 1, 2, 3, 4.
Colin has six six-sided (cubic) dice,
each with faces numbered 1, 2, 3, 4, 5, 6.

Peter and Colin roll their dice and compare totals: the highest total wins.
The result is a draw if the totals are equal.

What is the probability that Pyramidal Pete beats Cubic Colin?
Give your answer rounded to seven decimal places in the form 0.abcdefg
-}

type Dist a = [(a, Rational)]

peter1, colin1 :: Dist Int
peter1 = map (\x -> (x, 1/4)) [1 .. 4]
colin1 = map (\x -> (x, 1/6)) [1 .. 6]

merge_dist [] ys = ys
merge_dist xs [] = xs
merge_dist ((x,px):xs) ((y,py):ys) =
  case compare x y of
    EQ -> (x, px+py) : merge_dist xs ys
    LT -> (x, px) : merge_dist xs ((y,py):ys)
    GT -> (y, px) : merge_dist ((x,px):xs) ys

add_dist xs ys = foldr1 merge_dist (map f ys)
  where
    f (y,py) = [(x+y, px*py) | (x,px) <- xs]

peter n = foldr1 add_dist (replicate n peter1)
colin n = foldr1 add_dist (replicate n colin1)

less_dist [] ys = 0
less_dist xs [] = 0
less_dist ((x,px):xs) ((y,py):ys)
  | x < y = px * (py + sum (map snd ys))
            + less_dist xs ((y,py):ys)
  | otherwise = less_dist ((x,px):xs) ys

prob205 () = less_dist (colin 6) (peter 9)

-- 48679795/84934656 ~ 0.5731441

main :: IO String
main = return $ showFloat 7 $ less_dist (colin 6) (peter 9)

showFloat :: RealFrac a => Int -> a -> String
showFloat n x = s
  where
    y = floor x :: Integer
    z = round (x * 10^n) :: Integer
    s = show y ++ "." ++ reverse (take n (reverse (show z)))
