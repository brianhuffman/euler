module Euler321 where
import EulerLib (triangle)
import qualified SortedList as S

{-

Problem 321
23 January 2011

A horizontal row comprising of 2n + 1 squares has n red counters
placed at one end and n blue counters at the other end, being
separated by a single empty square in the centre. For example, when n
= 3.

+---+---+---+---+---+---+---+
| R | R | R |   | B | B | B |
+---+---+---+---+---+---+---+

A counter can move from one square to the next (slide) or can jump
over another counter (hop) as long as the square next to that counter
is unoccupied.
               _______
+---+---+   +-/-+---+-\-+
| R---> |   | R | B | v |
+---+---+   +---+---+---+

Let M(n) represent the minimum number of moves/actions to completely
reverse the positions of the coloured counters; that is, move all the
red counters to the right and all the blue counters to the left.

It can be verified M(3) = 15, which also happens to be a triangle
number.

If we create a sequence based on the values of n for which M(n) is a
triangle number then the first five terms would be: 1, 3, 10, 22, and
63, and their sum would be 99.

Find the sum of the first forty terms of this sequence.

-}

{-

-----
r_b 0
_rb 1
br_ 2
b_r 3

rbr
>{>

-------
rr_bb 0
r_rbb 1
rbr_b 2
rbrb_ 3
rb_br 4
_brbr 5
b_rbr 6
bbr_r 7
bb_rr 8

rbbrrbbr
>{<}}<{>

----------
rrr_bbb 0
rr_rbbb 1
rrbr_bb 2
rrbrb_b 3
rrb_brb 4
r_brbrb 5
_rbrbrb 6
br_rbrb 7
brbr_rb 8
brbrbr_ 9
brbrb_r 10
brb_brr 11
b_brbrr 12
bb_rbrr 13
bbbr_rr 14
bbb_rrr 15

rbbrrrbbbrrrbbr
>{<}}>{{{>}}<{>

-----------
rrrr_bbbb 0
rrr_rbbbb 1
rrrbr_bbb 2
rrrbrb_bb 3
rrrb_brbb 4
rr_brbrbb 5
r_rbrbrbb 6
rbr_rbrbb 7
rbrbr_rbb 8
rbrbrbr_b 9
rbrbrbrb_ 10
rbrbrb_br 11
rbrb_brbr 12
rb_brbrbr 13
_brbrbrbr 14
b_rbrbrbr 15
bbr_rbrbr 16
bbrbr_rbr 17
bbrbrbr_r 18
bbrbrb_rr 19
bbrb_brrr 20
bb_brbrrr 21
bbb_rbrrr 22
bbbbr_rrr 23
bbbb_rrrr 24

rbbrrrbbbbrrrrbbbbrrrbbr
>{<}}>{{{<}}}}<{{{>}}<{>

------------------------

M(1) = 3  = triangle 2
M(2) = 8
M(3) = 15  = triangle 5
M(4) = 24

Number of moves:  M(n) = n^2 + 2n
Triangle numbers: T(i) = (i^2 + i) / 2

M(n) = T(i)
n^2 + 2n = (i^2 + i) / 2
i is approximately sqrt(2)*n

M(1) = T(2)
M(3) = T(5)
M(10) = T(15)
...

From brute force search:
(0,0)
(1,2)
(3,5)
(10,15)
(22,32)
(63,90)
(133,189)
(372,527)
(780,1104)
(2173,3074)
(4551,6437)
(12670,17919)
(26530,37520)
(73851,104442)
(154633,218685)
(430440,608735)
(901272,1274592)
(2508793,3547970)
(5253003,7428869)
(14622322,20679087)

Find linear recurrence?
Adjacent ratios seem to bounce between 2 values.
Look for recurrence for sequence of every-other value.

Found it:
next (x,y) = (3*x+2*y+3, 4*x+3*y+5)

-}

moves n = n*(n+2)

slow_triangle_indices :: [(Integer, Integer)]
slow_triangle_indices = search 1 3 1 1
  where
    search n m i t =
      case compare m t of
        LT -> search n' m' i t
        EQ -> (n, i) : search n' m' i' t' 
        GT -> search n m i' t'
      where
        n' = n+1
        m' = moves n'
        i' = i+1
        t' = triangle i'

next (x,y) = (3*x+2*y+3, 4*x+3*y+5)

triangle_indices :: [(Integer, Integer)]
triangle_indices = (1,2) : (3,5) : map next triangle_indices

prob321 k = sum (map fst (take k triangle_indices))

main :: IO String
main = return $ show $ prob321 40

answer :: String
answer = "2470433131948040"
