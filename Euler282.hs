module Euler286 where
import Primes

{---------------------------------------------------------------------
Problem 282
The Ackermann Function

12 March 2010

For non-negative integers m, n, the Ackermann function A(m, n) is
defined as follows:

A(m,n) = n + 1             if m = 0
A(m,n) = A(m-1, 1)         if m > 0 and n = 0
A(m,n) = A(m-1, A(m,n-1))  if m > 0 and n > 0

For example A(1, 0) = 2, A(2, 2) = 7 and A(3, 4) = 125.

Find ∑(0 ≤ n ≤ 6) A(n, n) and give your answer mod 14^(8). 

---------------------------------------------------------------------}

{---------------------------------------------------------------------

A(m,n) = 2 \uparrow^{m-2} (n+3) - 3.

A(0, 0) = 1
A(1, 1) = 3  = 2 + (1 + 3) - 3
A(2, 2) = 7  = 2 * (2 + 3) - 3
A(3, 3) = 61 = 2 ^ (3 + 3) - 3
A(4, 4)      = 2 ^^ (4 + 3) - 3
A(5, 5)      = 2 ^^^ (5 + 3) - 3
A(6, 6)      = 2 ^^^^ (6 + 3) - 3

2 has order 2470629 = 3*7^7 (mod 7^8)
2 has order 705894 = 2*3*7^6 (mod 2470629)
2 has order 100842 = 2*3*7^5 (mod 352947)
2 has order 14406 = 2*3*7^4 (mod 50421)

A(4, 4) == 2614288 (mod 7^8)

The tetrations of 2 (mod 7^14) have a pattern: 10 initial values,
followed by a cycle of length 99.

---------------------------------------------------------------------}

tetrate a 1 = a
tetrate a b = a ^ tetrate a (b - 1)

-- a ^^ b (mod n)
tetrateMod a b n = go b
  where
    m = mult_order a n
    go 1 = a
    go b = expMod a (go (b-1) `mod` m) n
