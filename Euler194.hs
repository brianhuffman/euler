module Euler194 where
import EulerLib

-- 194. Coloured Configurations

-- Chromatic polynomials

{-
length (unitA 3) = 4 = 1 * (4 + 0 * ...)
length (unitA 4) = 62 = 2 * (4 + 1 * (27 + 0 * ...))
length (unitA 5) = 372 = 3 * (4 + 2 * (27 + 1 * (33 + 0 * ...)))
length (unitA 6) = 1396 = 4 * (4 + 3 * (27 + 2 * (33 + 1 * 11)))

length (unitB 3) = 6 = 1 * (6 + 0 * ...)
length (unitB 4) = 88 = 2 * (6 + 1 * (38 + ...))
length (unitB 5) = 486 = 3 * (6 + 2 * (38 + 1 * (40 + 0 * ...)))
length (unitB 6) = 1760 = 4 * (6 + 3 * (38 + 2 * (40 + 1 * 12)))
-}

unitA n =
  [ [a,b,c,d,e,f,g] |
    let a = 1,
    let b = 2,
    c <- [1 .. n],
    c /= a,
    d <- [1 .. n],
    d /= c,
    e <- [1 .. n],
    e /= b,
    e /= d,
    f <- [1 .. n],
    f /= a,
    f /= c,
    g <- [1 .. n],
    g /= b,
    g /= e,
    g /= f ]

unitB n =
  [ [a,b,c,d,e,f,g] |
    let a = 1,
    let b = 2,
    c <- [1 .. n],
    c /= a,
    d <- [1 .. n],
    d /= c,
    e <- [1 .. n],
    e /= b,
    e /= d,
    f <- [1 .. n],
    f /= a,
    f /= c,
    g <- [1 .. n],
    g /= e,
    g /= f ]

length_unitA n =
  (n-2) * (4 + (n-3) * (27 + (n-4) * (33 + (n-5) * (11 + (n-6)))))

length_unitB n =
  (n-2) * (6 + (n-3) * (38 + (n-4) * (40 + (n-5) * (12 + (n-6)))))

prob194 (a,b,c) = c * (c-1) * (na ^ a) * (nb ^ b) * choose (a+b) a
  where
    na = length_unitA c
    nb = length_unitB c

main :: IO String
main = return $ show $ prob194 (25,75,1984) `mod` (10^8)