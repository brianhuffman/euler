module Euler191 where

{-
Problem 191
26 April 2008

A particular school offers cash rewards to children with good attendance and
punctuality. If they are absent for three consecutive days or late on more
than one occasion then they forfeit their prize.

During an n-day period a trinary string is formed for each child consisting
of L's (late), O's (on time), and A's (absent).

Although there are eighty-one trinary strings for a 4-day period that can be
formed, exactly forty-three strings would lead to a prize:

OOOO OOOA OOOL OOAO OOAA OOAL OOLO OOLA OAOO OAOA
OAOL OAAO OAAL OALO OALA OLOO OLOA OLAO OLAA AOOO
AOOA AOOL AOAO AOAA AOAL AOLO AOLA AAOO AAOA AAOL
AALO AALA ALOO ALOA ALAO ALAA LOOO LOOA LOAO LOAA
LAOO LAOA LAAO

How many "prize" strings exist over a 30-day period?
-}

{-
Define P(n) = number of "prize" strings of length n
Define L(n) = number of "prize" strings of length n, not containing "L"

A prize string not containing "L" must have one of the forms
""
"A"
"AA"
"O" ++ l
"AO" ++ l
"AAO" ++ l
where l is another prize string not containing "L".

A prize string must have one of the forms
""
"A"
"AA"
"O" ++ p
"AO" ++ p
"AAO" ++ p
"L" ++ l
"AL" ++ l
"AAL" ++ l

L(0) = 1
L(1) = 2
L(2) = 4
L(n) = L(n-1) + L(n-2) + L(n-3)

P(0) = 1
P(1) = 3
P(2) = 8
P(n) = L(n) + P(n-1) + P(n-2) + P(n-3)
-}

prob191 :: Int -> Integer
prob191 n = f 1 0 0 0 0 0 !! n
  where
    f l1 l2 l3 p1 p2 p3 = p0 : f l0 l1 l2 p0 p1 p2
      where
        l0 = l1 + l2 + l3
        p0 = p1 + p2 + p3 + l0

main :: IO String
main = return $ show $ prob191 30
-- 1918080160
