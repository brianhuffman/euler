module Euler330 where
import Primes

{----------------------------------------------------------------------

Problem 330
27 March 2011

An infinite sequence of real numbers a(n) is defined for all integers
n as follows:

a(n) = 1, for n < 0
a(n) = SUM i=[1..], a(n-i)/i!, for 0 <= n

For example,
a(0) = 1/1! + 1/2! + 1/3! + ... = e − 1
a(1) = (e − 1) / 1! + 1/2! + 1/3! + ... = 2e − 3
a(2) = (2e − 3) / 1! + (e − 1) / 2! + 1 / 3! + ... = 7/2 (e − 6)

with e = 2.7182818... being Euler's constant.

It can be shown that a(n) is of the form (A(n) e + B(n)) / n! for
integers A(n) and B(n). For example

a(10) = (328161643 e − 652694486) / 10! .

Find A(10^9) + B(10^9) and give your answer mod 77 777 777.

----------------------------------------------------------------------}

{---------------------------------------------------------------------
a(0) = (e − 1) / 0!
a(1) = (2e − 3) / 1!
a(2) = (7e − 6) / 2!
a(3) = 
a(10) = (328161643 e − 652694486) / 10!

(SUM i=[1..], 1/i!) = 1 - e

a(n) = SUM i=[1..], a(n-i)/i!
     = (1-e) + (SUM i=[1..n], (a(n-i) - 1)/i!)
     = (1-e) + (SUM j=[0..n-1], (a(j) - 1)/(n-j)!)    j=n-i, i=n-j


a(n) = (A(n) e + B(n)) / n!
a(n) n! = A(n) e + B(n)


a(n) = (e-1) + SUM j=[0..n-1], (a(j)-1)/(n-j)!
a(n) n! = (e-1)n! + SUM j=[0..n-1], (a(j)-1) n!/(n-j)!
A(n) e + B(n) = (e-1)n! + SUM j=[0..n-1], (a(j)-1) n!/(n-j)!

A(n) e + B(n) = (e-1)n! + SUM j=[0..n-1], (A(j)e/j! + B(j)/j! - 1) n!/(n-j)!


A(n) = n! + SUM j=[0..n-1], A(j) n!/j!(n-j)!
B(n) = (-1)n! + SUM j=[0..n-1], B(j) n!/j!(n-j)! - n!/(n-j)!

A(n) = n! + SUM j=[0..n-1], A(j) choose(n,j)
B(n) = (-1)n! + SUM j=[0..n-1], B(j) choose(n,j) - n!/(n-j)!

A(n) = n! + SUM j=[0..n-1], A(j) choose(n,j)
B(n) = (-1)n! + [SUM j=[0..n-1], B(j) choose(n,j)] - [SUM j=[0..n-1], n!/(n-j)!]
-- Take just "e" part:
A(0) = 0!
A(1) = 1! + A(0) C(1,0)
A(2) = 2! + A(0) C(2,0) + A(1) C(2,1)
A(3) = 3! + A(0) C(3,0) + A(1) C(3,1) + A(2) C(3,2)
A(4) = 4! + A(0) C(4,0) + A(1) C(4,1) + A(2) C(4,2) + A(3) C(4,3)

-- Take just integer part:
B(0) = - (0!/0!)
B(1) = (B(0) C(1,0)) - (1!/0! + 1!/1!)
B(2) = (B(0) C(2,0) + B(1) C(2,1)) - (2!/0! + 2!/1! + 2!/2!)
B(3) = (B(0) C(3,0) + B(1) C(3,1) + B(2) C(3,2)) - (3!/0! + 3!/1! + 3!/2! + 3!/3!)

-- Recursive formulas for A and B:

A(n) = n! + [SUM j=[0..n-1], A(j) choose(n,j)]
B(n) = [SUM j=[0..n-1], B(j) choose(n,j)] - [SUM j=[0..n], n!/j!]

----------------------------------------------------------------------

If we remember that C(n,k) = C(n,k-1) + C(n-1,k-1), then we can
rewrite this to

A(3) = 3! + A(0) C(3,0) + A(1) C(3,1) + A(2) C(3,2)
A(4) = 4! + A(0) C(4,0) + A(1) C(4,1) + A(2) C(4,2) + A(3) C(4,3)

A(4) = 4! + A(0) C(3,0) + A(1) C(3,1) + A(2) C(3,2) + A(3) C(3,3)
                        + A(1) C(3,0) + A(2) C(3,1) + A(3) C(3,2)


A(4) = 4! + A(3) - 3! + A(3) C(3,3)
                        + A(1) C(3,0) + A(2) C(3,1) + A(3) C(3,2)

A(4) = 4! - 3! + 2 A(3) + A(1) C(3,0) + A(2) C(3,1) + A(3) C(3,2)

A(4) = 4! - 3! + 2 A(3)
  + A(1) C(3,0) + A(2) C(3,1) + A(3) C(3,2)



---------------------------------------------------------------------}

fact n = product [1..n]

a_seq :: [Integer]
a_seq = xs
  where
    xs = f 0 1 []
    f n nfac row = x : f n' nfac' row'
      where
        x = sum (zipWith (*) xs row) + nfac
        n' = n+1
        nfac' = nfac*n'
        row' = zipWith (+) ([0] ++ row) (row ++ [1])

-- A(n) = (-1)n! + SUM j=[0..n-1], A(j) choose(n,j)

b_seq :: [Integer]
b_seq = xs
  where
    xs = f 0 1 []
    f n nfacs row = x : f n' nfacs' row'
      where
        x = sum (zipWith (*) xs row) - nfacs
        n' = n+1
        nfacs' = nfacs*n' + 1
        row' = zipWith (+) ([0] ++ row) (row ++ [1])

{---------------------------------------------------------------------

By experiment, we see that 2*A(n) + B(n) = n! for all n.

So we could calculate either one from the other.

A(n) = (n! - B(n)) / 2
B(n) = n! - 2*A(n)

0: 1 e - 1
1: 2 e - 3
2: 7 e - 12
3: 34 e - 62
4: 211 e - 398
5: 1596 e - 3072
...

----------------------------------------------------------------------

A(n) = n! + [SUM j=[0..n-1], A(j) choose(n,j)]

A(0) = 0!
A(1) = 1! + 1 A(0)
A(2) = 2! + 1 A(0) + 2 A(1)
A(3) = 3! + 1 A(0) + 3 A(1) +  3 A(2)
A(4) = 4! + 1 A(0) + 4 A(1) +  6 A(2) +  4 A(3)
A(5) = 5! + 1 A(0) + 5 A(1) + 10 A(2) + 10 A(3) +  5 A(4)
A(6) = 6! + 1 A(0) + 6 A(1) + 15 A(2) + 20 A(3) + 15 A(4) + 6 A(5)

A(0) =  1⋅0!
A(1) =  1⋅0! +  1⋅1!
A(2) =  3⋅0! +  1⋅1! +  1⋅2!
A(3) = 13⋅0! +  6⋅1! +  3⋅2! + 1⋅3!
A(4) = 75⋅0! + 34⋅1! + 18⋅2! + 4⋅3! + 1⋅4!

---------------------------------------------------------------------}

-- map (`mod` m) a_seq
a_seq_mod :: Integer -> [Integer]
a_seq_mod m = xs
  where
    xs = f 0 1 []
    f n nfac row = seq x (x : f n' nfac' row')
      where
        x = (sum (zipWith (*) row xs) + nfac) `mod` m
        n' = n+1
        nfac' = (nfac * n') `mod` m
        row' = map (`mod` m) (zipWith (+) ([0] ++ row) (row ++ [1]))

-- map (`mod` m) a_seq
b_seq_mod :: Integer -> [Integer]
b_seq_mod m = xs
  where
    xs = f 0 1 []
    f n nfacs row = seq x (x : f n' nfacs' row')
      where
        x = (sum (zipWith (*) row xs) - nfacs) `mod` m
        n' = n+1
        nfacs' = (nfacs * n' + 1) `mod` m
        row' = map (`mod` m) (zipWith (+) ([0] ++ row) (row ++ [1]))

{-
alternates odd and even:
A(2n) is odd, A(2n+1) is even

mod 2, it has cycle length 2.
mod 3, it has cycle length 6, after initial 3

mod 7, it has cycle length 42, after initial 7
mod 11, it has cycle length 110, after initial 11
mod 73, it has cycle length (72*73), after initial 73
mod 101, it has cycle length 10100, after initial 101
mod 137, it has cycle length 137*136, after initial 137

in general:
mod p^k, it has cycle length (p-1)*p^k, after some initial <= p^k.

77777777 = 7 * 11 * 73 * 101 * 137

10^9 == 34 (mod 42)
10^9 == 10 (mod 110)
10^9 == 3952 (mod 5256)
10^9 == 9100 (mod 10100)
10^9 == 1928 (mod 18632)

A(10^9) == A(34) == 6 (mod 7)
A(10^9) == A(120) == 8 (mod 11)
A(10^9) == A(3952) == 7 (mod 73)
A(10^9) == A(9100) == 57 (mod 101)
A(10^9) == A(1928) == 20 (mod 137)
A(10^9) == 61821955 (mod 77777777)

B(10^9) == B(34) == 2 (mod 7)
B(10^9) == B(120) == 6 (mod 11)
B(10^9) == B(3952) == 59 (mod 73)
B(10^9) == B(9100) == 88 (mod 101)
B(10^9) == B(1928) == 97 (mod 137)
B(10^9) == 31911644 (mod 77777777)

A(10^9) + B(10^9) == 15955822 (mod 77777777)

Shortcut: A(n) + B(n) = n! - A(n)
For large n, n! == 0 (mod m).
-}

a7s = foldr1 chinese [(6,7),(8,11),(7,73),(57,101),(20,137)]
b7s = foldr1 chinese [(2,7),(6,11),(59,73),(88,101),(97,137)]

prob330 :: Integer -> Integer -> Integer
prob330 n m | m < n = m - fst (foldr1 chinese (map f pf))
  where
    pf = prime_factorization m
    f (p, k) = (a_seq_mod pk !! fromIntegral n', pk)
      where
        pk = p ^ k
        period = (p-1)*pk
        n' = ((n - pk) `mod` period) + pk

main :: IO String
main = return $ show $ prob330 (10^9) 77777777

answer :: String
answer = "15955822"
