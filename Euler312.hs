module Euler312 where
import Primes

{-

Problem 312
Cyclic paths on Sierpiński graphs
28 November 2010

- A Sierpiński graph of order-1 (S[1]) is an equilateral triangle.

- S[n+1] is obtained from S[n] by positioning three copies of S[n] so
that every pair of copies has one common corner.

                             *
                            / \
                           *---*
                          / \ / \
             *           *---*---*
            / \         / \     / \
  *        *---*       *---*   *---*
 / \      / \ / \     / \ / \ / \ / \
*---*    *---*---*   *---*---*---*---*
  S1         S2              S3

                *
               / \
              *---*
             / \ / \
            *---*---*
           / \     / \
          *---*   *---*
         / \ / \ / \ / \
        *---*---*---*---*
       / \             / \
      *---*           *---*
     / \ / \         / \ / \
    *---*---*       *---*---*
   / \     / \     / \     / \
  *---*   *---*   *---*   *---*
 / \ / \ / \ / \ / \ / \ / \ / \
*---*---*---*---*---*---*---*---*
                S4

Let C(n) be the number of cycles that pass exactly once through all
the vertices of S[n]. For example, C(3) = 8 because eight such cycles
can be drawn on S3, as shown below:

It can also be verified that :
C(1) = C(2) = 1
C(5) = 71328803586048
C(10 000) mod 10^8 = 37652224
C(10 000) mod 13^8 = 617720485

Find C(C(C(10 000))) mod 13^8.

-}



{-

Let D(n) be the number of paths that pass through all the vertices of
S[n], starting in bottom-left corner and ending in bottom-right corner.

C(n+1) = D(n)^3.

Let E(n) be the number of paths that pass through all the vertices of
S[n] *except* for the top one, going from bottom-left to bottom-right.

D(1) = 1.
D(2) = 2. (See diagrams below.)
D(n+1) = 2 * D(n)^2 * E(n)
    *           *
   / \         / \
  *   *       *   *
   \   \     /   /
*---*   *   *   *---*

E(1) = 1.
E(2) = 3. (See diagrams below.)
E(n+1) = 2 * D(n) * E(n)^2

  *   *       *---*       *---*
 / \ / \     /   /         \   \
*   *   *   *   *---*   *---*   *

C(n), D(n) and E(n) consist purely of powers of 2 and 3.
We will use the notation (a,b) to mean 2^a*3^b.

n |  C(n)  |  D(n)  |  E(n)  | 
------------------------------
1 | (0,0)    (0,0)    (0,0)
2 | (0,0)    (1,0)    (0,1)
3 | (3,0)    (3,1)    (2,2)
4 | (9,3)    (9,4)    (8,5)
5 | (27,12)  (27,13)  (26,14)

Is it always the case for n>1 that 3*D(n) = 2*E(n)?
Proof. Base case (n=2): 3*D(2) = 6 = 2*E(2).
Case (n+1): Assume that 3*D(n) = 2*E(n). Then
(3*D(n))*(2*D(n)*E(n)) = (2*E(n))*(2*D(n)*E(n))
     3*(2*D(n)^2*E(n)) = 2*(2*D(n)*E(n)^2)
              3*D(n+1) = 2*E(n+1).
QED.

Using this, we can rewrite the rule for D(n+1):

D(n+1) = 2 * D(n)^2 * E(n)
D(n+1) = D(n)^2 * (2*E(n))
D(n+1) = D(n)^2 * (3*D(n))
D(n+1) = 3*D(n)^3
       = 3*C(n+1)

Also, this means that D(n) = 3*C(n) for n>2.

So we can rewrite the rule for C(n+1):

C(1) = 1
C(2) = 1
C(3) = 8
C(n+1) = 3^3 * C(n)^3

n |  C(n)
------------------------------
1 | (0,0)
2 | (0,0)
3 | (3,0)
4 | (9,3)
5 | (27,12)
6 | (81,39)
7 | (243,120)

Written in base 3:
n |  C(n)
------------------
3 | (10,0)
4 | (100,10)
5 | (1000,110)
6 | (10000,1110)
7 | (100000,11110)

For n > 2,
C(n) = 2^(3^(n-2)) * 3^((3^(n-2)-3)/2)
C(n) = 8 * 12^((3^(n-2)-3)/2)

----------------------------------------------------------------------
Find C(C(C(10 000))) mod 13^8.

C(n) == C(n') (mod 13^8)

8 * 12^((3^(n-2)-3)/2) == 8 * 12^((3^(n'-2)-3)/2) (mod 13^8)
12^((3^(n-2)-3)/2) == 12^((3^(n'-2)-3)/2) (mod 13^8)
   -- 12^(2*13^7) == 1 (mod 13^8)
(3^(n-2)-3)/2 == (3^(n'-2)-3)/2 (mod 2*13^7)
3^(n-2)-3 == 3^(n'-2)-3 (mod 4*13^7)
3^(n-2) == 3^(n'-2) (mod 4*13^7)
   -- 3^(6*13^6) == 1 (mod 4*13^7)
n-2 == n'-2 (mod 6*13^6)
n == n' (mod 6*13^6)

So, in order to calculate C(C(C(10 000))) mod 13^8,
we must first find C(C(10 000)) mod 6*13^6.
----------------------------------------------------------------------
Find C(C(10 000)) mod 6*13^6.

C(n) == C(n') (mod 6*13^6)
8 * 12^((3^(n-2)-3)/2) == 8 * 12^((3^(n'-2)-3)/2) (mod 6*13^6)
4 * 12^((3^(n-2)-3)/2) == 4 * 12^((3^(n'-2)-3)/2) (mod 3*13^6)
12^((3^(n-2)-3)/2) == 12^((3^(n'-2)-3)/2) (mod 3*13^6)
    -- assuming that n > 3, both sides will be multiples of 3.
12^((3^(n-2)-3)/2) == 12^((3^(n'-2)-3)/2) (mod 13^6)
    -- 12^(2*13^5) == 1 (mod 13^6)
(3^(n-2)-3)/2 == (3^(n'-2)-3)/2 (mod 2*13^5)
3^(n-2)-3 == 3^(n'-2)-3 (mod 4*13^5)
3^(n-2) == 3^(n'-2) (mod 4*13^5)
   -- 3^(6*13^4) == 1 (mod 4*13^5)
n-2 == n'-2 (mod 6*13^4)
n == n' (mod 6*13^4)

So, in order to calculate C(C(10 000)) mod 6*13^6,
we must first find C(10 000) mod 6*13^4.
----------------------------------------------------------------------
Find C(10 000) mod 6*13^4.

C(n) == C(n') (mod 6*13^4)
8 * 12^((3^(n-2)-3)/2) == 8 * 12^((3^(n'-2)-3)/2) (mod 6*13^4)
4 * 12^((3^(n-2)-3)/2) == 4 * 12^((3^(n'-2)-3)/2) (mod 3*13^4)
12^((3^(n-2)-3)/2) == 12^((3^(n'-2)-3)/2) (mod 3*13^4)
    -- assuming that n > 3, both sides will be multiples of 3.
12^((3^(n-2)-3)/2) == 12^((3^(n'-2)-3)/2) (mod 13^4)
    -- 12^(2*13^3) == 1 (mod 13^4)
(3^(n-2)-3)/2 == (3^(n'-2)-3)/2 (mod 2*13^3)
3^(n-2)-3 == 3^(n'-2)-3 (mod 4*13^3)
3^(n-2) == 3^(n'-2) (mod 4*13^3)
   -- 3^(6*13^2) == 1 (mod 4*13^3)
n-2 == n'-2 (mod 6*13^2)
n == n' (mod 6*13^2)

So, in order to calculate C(10 000) mod 6*13^4,
we must first find 10 000 mod 6*13^2.
----------------------------------------------------------------------
10000 == 874 (mod 6*13^2)

C(10000) == C(874) == 88860 (mod 6*13^4)

C(C(10000)) == C(88860) == 2792886 (mod 6*13^6)

C(C(C(10000))) == C(2792886) == 10939362 (mod 13^8)

10000 `mod` (6*13^2) = 874
c_mod (6*13^4) 874 = 88860
c_mod (6*13^6) 88860 = 2792886
c_mod (13^8) 2792886 = 324681947

-}

c n
  | n <= 2 = 1
  | otherwise = 2^k * 3^((k-3)`div`2)
  where k = 3^(n-2)

c' n
  | n <= 2 = 1
  | otherwise = 8 * 12^k
  where k = (3^(n-2)-3)`div`2
{-
c_mod m n
  | n <= 2 = 1
  | otherwise = (expMod 2 k m * expMod 3 ((k-3)`div`2) m) `mod` m
  where k = 3^(n-2)
-}

c_mod m n
  | n <= 2 = 1
  | otherwise = (8 * expMod 12 (k `mod` totient m) m) `mod` m
  where k = (3^(n-2)-3)`div`2

{-

2^(12*13^7) == 1 (mod 13^8).
3^(3*13^7) == 1 (mod 13^8).

-}

-- C(C(C(n))) mod 13^8
prob312 :: Integer -> Integer
prob312 n =
  c_mod (13^8) (c_mod (6*13^6) (c_mod (6*13^4) (n `mod` (6*13^2))))

main :: IO String
main = return $ show $ prob312 10000

answer :: String
answer = "324681947"
