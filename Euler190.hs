module Euler190 where
import EulerLib
import Ratio

{-
Problem 190
18 April 2008

Let Sm = (x1, x2, ... , xm) be the m-tuple of positive real numbers with
x1 + x2 + ... + xm = m for which Pm = x1 * x2^2 * ... * xm^m is maximised.

For example, it can be verified that [P10] = 4112 ([ ] is the integer part
function).

Find Σ[Pm] for 2 <= m <= 15.
-}

{-
For vector x = (x1, x2, ... , xm),
define s(x) = x1 + x2 + ... + xm
and p(x) = x1 * x2^2 * ... * xm^m

We must find x that maximizes p(x), under the constraint s(x) = m.

Note that p(x) is maximized iff ln(p(x)) is maximized.

ln(p(x)) = ln(x1) + 2 ln(x2) + ... + m ln(xm)

ln(p(x)) is maximized when its gradient is parallel to the gradient
of s(x). That is, all the partial derivatives of ln(p(x)) must be equal.

d/dx (ln x) = 1/x

d/dxi ln(p(x)) = i / xi

1/x1 = 2/x2 = ... = m/xm

For m = 2, x = [2/3, 4/3]

For m = 3, x = [1/2, 2/2, 3/2]
-}

coeffs n = [ a * n % d | a <- [1 .. n] ]
  where d = sum [1 .. n]

poly xs = product $ zipWith (^) xs [1 ..]

prob190 n = floor (poly (coeffs n))

main :: IO String
main = return $ show $ sum [ prob190 n | n <- [2 .. 15] ]
-- 371048281
