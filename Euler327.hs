module Euler327 where

{-

Problem 327
05 March 2011

A series of three rooms are connected to each other by automatic doors.

           +-------+-------+-------+
  ---------|       |       |       |----------
    Start  =   1   =   2   =   3   =  Finish
  ---------|       |       |       |----------
           +-------+-------+-------+

Each door is operated by a security card. Once you enter a room the
door automatically closes and that security card cannot be used again.
A machine at the start will dispense an unlimited number of cards, but
each room (including the starting room) contains scanners and if they
detect that you are holding more than three security cards or if they
detect an unattended security card on the floor, then all the doors
will become permanently locked. However, each room contains a box
where you may safely store any number of security cards for use at a
later stage.

If you simply tried to travel through the rooms one at a time then as
you entered room 3 you would have used all three cards and would be
trapped in that room forever!

However, if you make use of the storage boxes, then escape is
possible. For example, you could enter room 1 using your first card,
place one card in the storage box, and use your third card to exit the
room back to the start. Then after collecting three more cards from
the dispensing machine you could use one to enter room 1 and collect
the card you placed in the box a moment ago. You now have three cards
again and will be able to travel through the remaining three doors.
This method allows you to travel through all three rooms using six
security cards in total.

It is possible to travel through six rooms using a total of 123
security cards while carrying a maximum of 3 cards.

Let C be the maximum number of cards which can be carried at any time.

Let R be the number of rooms to travel through.

Let M(C,R) be the minimum number of cards required from the dispensing
machine to travel through R rooms carrying up to a maximum of C cards
at any time.

For example, M(3,6)=123 and M(4,6)=23.
And, ΣM(C,6)=146 for 3 ≤ C ≤ 4.

You are given that ΣM(C,10)=10382 for 3 ≤ C ≤ 10.

Find ΣM(C,30) for 3 ≤ C ≤ 40.

-}


{-

Maximum of 3 cards:
M(3,0) = 1  (straight through)
M(3,1) = 2  (straight through)
M(3,2) = 3  (straight through)
M(3,3) = 6 = 3+1*2+1 (1 pre-trip to stock room 1)
M(3,4) = 15 = 6+4*2+1 (4 pre-trips to stock room 1)
M(3,5) = 42 = 15+13*2+1 (13 pre-trips to stock room 1)
M(3,6) = 123 = 42+40*2+1 (40 pre-trips to stock room 1)
M(3,n+1) = 3*M(3,n) - 3

Maximum of 4 cards:
M(4,0) = 1  (straight through)
M(4,1) = 2  (straight through)
M(4,2) = 3  (straight through)
M(4,3) = 4  (straight through)
M(4,4) = 7 = 4+1*2+1 (1 pre-trip to stock room 1 with 1 card)
M(4,5) = 12 = 7+2*2+1 (2 pre-trips to stock room 1 with 4 cards)
M(4,6) = 23 = 12+5*2+1 (5 pre-trips to stock room 1 with 9 cards)

Maximum of 5 cards:

              3  3  3
 3  3  3  3  / \/ \/ \
/ \/ \/ \/ \/         \ 16 steps to deliver 9 cards through 2 doors

  1    1    1
 / \  / \  / \
/   \/   \/   \...  36 steps to deliver 9 cards through 2 doors

It is most efficient to always have a "full wallet" when moving forward.

-}

prob327 :: Integer -> Integer -> Integer
prob327 c r | r < c = r + 1
prob327 c r = n + 2*((n-c)`div`(c-2)) + 3
  where n = prob327 c (r-1)

main :: IO String
main = return $ show $ sum [ prob327 c 30 | c <- [3..40] ]

answer :: String
answer = "34315549139516"
