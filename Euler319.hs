module Euler319 where
import Data.List (minimumBy)

{-

Problem 319
08 January 2011

Let x[1], x[2],..., x[n] be a sequence of length n such that:

    * x[1] = 2
    * for all 1 < i ≤ n : x[i-1] < x[i]
    * for all i and j with 1 ≤ i, j ≤ n : (x[i])^j < (x[j] + 1)^i

There are only five such sequences of length 2, namely: {2,4}, {2,5},
{2,6}, {2,7} and {2,8}.

There are 293 such sequences of length 5; three examples are given
below: {2,5,11,25,55}, {2,6,14,36,88}, {2,8,22,64,181}.

Let t(n) denote the number of such sequences of length n.
You are given that t(10) = 86195 and t(20) = 5227991891.

Find t(10^10) and give your answer modulo 10^9.

-}


{-

s(i) = how many times will position i be incremented
s(i) = 3^i - 2^i - 1
s(1) = 0
s(2) = 4
s(3) = 18
s(4) = 64
s(5) = 210
s(6) = 664
s(7) = 2058
s(8) = 6304
s(9) = 19170
s(10) = 58024
s(11) = 175098
s(12) = 527344
s(13) = 1586130

t(1)  =       1 = s(1) + 1
t(2)  =       5 = t(1) + s(2)
t(3)  =      23 = t(2) + s(3)
t(4)  =      83 = t(3) + s(4) - s(2)
t(5)  =     293 = t(4) + s(5)
t(6)  =     935 = t(5) + s(6) - s(3) - s(2)
t(7)  =    2993 = t(6) + s(7)
t(8)  =    9233 = t(7) + s(8) - s(4)
t(9)  =   28385 = t(8) + s(9) - s(3)
t(10) =   86195 = t(9) + s(10) - s(5) - s(2)
t(11) =  261293 = t(10) + s(11)
t(12) =  787913 = t(11) + s(12) - s(6) - s(4) + s(2)
t(13) = 2374043 = t(12) + s(13)

t(1) = 1
t(2) = s(2) + 1
t(3) = s(3) + s(2) + 1
t(4) = s(4) + s(3) + 1
t(5) = s(5) + s(4) + s(3) + 1
t(6) = s(6) + s(5) + s(4) - s(2) + 1
t(7) = s(7) + s(6) + s(5) + s(4) - s(2) + 1
t(8) = s(8) + s(7) + s(6) + s(5) - s(2) + 1
t(9) = s(9) + s(8) + s(7) + s(6) + s(5) - s(3) - s(2) + 1
t(10) = s(10) + s(9) + s(8) + s(7) + s(6) - s(3) - 2s(2) + 1
t(11) = s(11) + s(10) + s(9) + s(8) + s(7) + s(6) - s(3) - 2s(2) + 1
t(12) = s(12) + s(11) + s(10) + s(9) + s(8) + s(7) - s(4) - s(3) - s(2) + 1
t(13) = s(13) + s(12) + s(11) + s(10) + s(9) + s(8) + s(7) - s(4) - s(3) - s(2) + 1

-}

all319 :: Int -> [[Integer]]
all319 1 = [[2]]
all319 n =
  [ xs ++ [y] |
    xs <- all319 (n-1),
    let ymin = max (2^n) (last xs + 1),
    y <- [ymin .. 3^n-1],
    all (\(i,x) -> x^n < (y+1)^i && y^i < (x+1)^n) (zip [1..n-1] xs)
  ]

next319 :: [Integer] -> [Integer]
next319 xs = zipWith (+) xs ds
  where
    zs = zip [1..] xs
    c (i,x) (j,y) = compare ((x+1)^j) ((y+1)^i)
    m = minimumBy c zs
    ds = map (\z -> if c m z == EQ then 1 else 0) zs

all319' :: Int -> [[Integer]]
all319' n = takeWhile (\xs -> head xs == 2) (iterate next319 xs0)
  where xs0 = take n (iterate (*2) 2)

s319 :: Integer -> Integer
s319 n = 3^n - 2^n - 1

{-

* Whichever xi has smallest xi^(1/i) is incremented next.
* Positions i and j increment together if i divides j.

All sequences of length 5:
[2,4,8,16,32]
[2,4,8,16,33]
[2,4,8,16,34]
[2,4,8,17,34]
[2,4,8,17,35]
[2,4,8,17,36]
[2,4,8,17,37] 4
[2,4,8,18,37]
[2,4,8,18,38] 3
[2,4,9,18,38]
[2,4,9,18,39] 4
[2,4,9,19,39]
[2,4,9,19,40]
[2,4,9,19,41]
[2,4,9,19,42] 4
[2,4,9,20,42]
[2,4,9,20,43]
[2,4,9,20,44] 4
[2,4,9,21,44]
[2,4,9,21,45]
[2,4,9,21,46] 3
[2,4,10,21,46]
[2,4,10,21,47] 4
[2,4,10,22,47]
[2,4,10,22,48]
[2,4,10,22,49]
[2,4,10,22,50] 4
[2,4,10,23,50]
[2,4,10,23,51]
[2,4,10,23,52]
[2,4,10,23,53] 4
[2,4,10,24,53]
[2,4,10,24,54] 3
[2,4,11,24,54]
[2,4,11,24,55] 2+4
[2,5,11,25,55]
[2,5,11,25,56]
[2,5,11,25,57]
[2,5,11,25,58] 4
[2,5,11,26,58]
[2,5,11,26,59]
[2,5,11,26,60]
[2,5,11,26,61] 4
[2,5,11,27,61]
[2,5,11,27,62] 3
[2,5,12,27,62]
[2,5,12,27,63]
[2,5,12,27,64] 4
[2,5,12,28,64]
[2,5,12,28,65]
[2,5,12,28,66]
[2,5,12,28,67] 4
[2,5,12,29,67]
[2,5,12,29,68]
[2,5,12,29,69]
[2,5,12,29,70] 4
[2,5,12,30,70]
[2,5,12,30,71] 3
[2,5,13,30,71]
[2,5,13,30,72]
[2,5,13,30,73] 4
[2,5,13,31,73]
[2,5,13,31,74]
[2,5,13,31,75]
[2,5,13,31,76] 4
[2,5,13,32,76]
[2,5,13,32,77]
[2,5,13,32,78]
[2,5,13,32,79] 4
[2,5,13,33,79]
[2,5,13,33,80]
[2,5,13,33,81] 3
[2,5,14,33,81]
[2,5,14,33,82] 4
[2,5,14,34,82]
[2,5,14,34,83]
[2,5,14,34,84]
[2,5,14,34,85] 4
[2,5,14,35,85]
[2,5,14,35,86]
[2,5,14,35,87]
[2,5,14,35,88] 2+4
[2,6,14,36,88]
[2,6,14,36,89]
[2,6,14,36,90]
[2,6,14,36,91] 3
[2,6,15,36,91] 4
[2,6,15,37,91]
[2,6,15,37,92]
[2,6,15,37,93]
[2,6,15,37,94] 4
[2,6,15,38,94]
[2,6,15,38,95]
[2,6,15,38,96]
[2,6,15,38,97] 4
[2,6,15,39,97]
[2,6,15,39,98]
[2,6,15,39,99]
[2,6,15,39,100] 4
[2,6,15,40,100]
[2,6,15,40,101] 3
[2,6,16,40,101]
[2,6,16,40,102]
[2,6,16,40,103] 4
[2,6,16,41,103]
[2,6,16,41,104]
[2,6,16,41,105]
[2,6,16,41,106] 4
[2,6,16,42,106]
[2,6,16,42,107]
[2,6,16,42,108]
[2,6,16,42,109]
[2,6,16,42,110] 4
[2,6,16,43,110]
[2,6,16,43,111]
[2,6,16,43,112] 3
[2,6,17,43,112]
[2,6,17,43,113] 4
[2,6,17,44,113]
[2,6,17,44,114]
[2,6,17,44,115]
[2,6,17,44,116] 4
[2,6,17,45,116]
[2,6,17,45,117]
[2,6,17,45,118]
[2,6,17,45,119] 4
[2,6,17,46,119]
[2,6,17,46,120]
[2,6,17,46,121]
[2,6,17,46,122]
[2,6,17,46,123] 4
[2,6,17,47,123] 3
[2,6,18,47,123]
[2,6,18,47,124]
[2,6,18,47,125]
[2,6,18,47,126] 4
[2,6,18,48,126]
[2,6,18,48,127]
[2,6,18,48,128]
[2,6,18,48,129] 2+4
[2,7,18,49,129]
[2,7,18,49,130]
[2,7,18,49,131]
[2,7,18,49,132] 4
[2,7,18,50,132]
[2,7,18,50,133]
[2,7,18,50,134]
[2,7,18,50,135] 3
[2,7,19,50,135]
[2,7,19,50,136] 4
[2,7,19,51,136]
[2,7,19,51,137]
[2,7,19,51,138]
[2,7,19,51,139] 4
[2,7,19,52,139]
[2,7,19,52,140]
[2,7,19,52,141]
[2,7,19,52,142]
[2,7,19,52,143] 4
[2,7,19,53,143]
[2,7,19,53,144]
[2,7,19,53,145]
[2,7,19,53,146] 4
[2,7,19,54,146]
[2,7,19,54,147] 3
[2,7,20,54,147]
[2,7,20,54,148]
[2,7,20,54,149] 4
[2,7,20,55,149]
[2,7,20,55,150]
[2,7,20,55,151]
[2,7,20,55,152]
[2,7,20,55,153] 4
[2,7,20,56,153]
[2,7,20,56,154]
[2,7,20,56,155]
[2,7,20,56,156] 4
[2,7,20,57,156]
[2,7,20,57,157]
[2,7,20,57,158]
[2,7,20,57,159] 3
[2,7,21,57,159]
[2,7,21,57,160] 4
[2,7,21,58,160]
[2,7,21,58,161]
[2,7,21,58,162]
[2,7,21,58,163] 4
[2,7,21,59,163]
[2,7,21,59,164]
[2,7,21,59,165]
[2,7,21,59,166] 4
[2,7,21,60,166]
[2,7,21,60,167]
[2,7,21,60,168]
[2,7,21,60,169]
[2,7,21,60,170]
[2,7,21,61,170]
[2,7,21,61,171]
[2,7,21,61,172]
[2,7,22,61,172]
[2,7,22,61,173]
[2,7,22,62,173]
[2,7,22,62,174]
[2,7,22,62,175]
[2,7,22,62,176]
[2,7,22,62,177]
[2,7,22,63,177]
[2,7,22,63,178]
[2,7,22,63,179]
[2,7,22,63,180]
[2,7,22,63,181]
[2,8,22,64,181]
[2,8,22,64,182]
[2,8,22,64,183]
[2,8,22,64,184]
[2,8,22,65,184]
[2,8,22,65,185]
[2,8,22,65,186]
[2,8,23,65,186]
[2,8,23,65,187]
[2,8,23,65,188]
[2,8,23,66,188]
[2,8,23,66,189]
[2,8,23,66,190]
[2,8,23,66,191]
[2,8,23,67,191]
[2,8,23,67,192]
[2,8,23,67,193]
[2,8,23,67,194]
[2,8,23,67,195]
[2,8,23,68,195]
[2,8,23,68,196]
[2,8,23,68,197]
[2,8,23,68,198]
[2,8,23,69,198]
[2,8,23,69,199]
[2,8,24,69,199]
[2,8,24,69,200]
[2,8,24,69,201]
[2,8,24,69,202]
[2,8,24,70,202]
[2,8,24,70,203]
[2,8,24,70,204]
[2,8,24,70,205]
[2,8,24,70,206]
[2,8,24,71,206]
[2,8,24,71,207]
[2,8,24,71,208]
[2,8,24,71,209]
[2,8,24,72,209]
[2,8,24,72,210]
[2,8,24,72,211]
[2,8,24,72,212]
[2,8,24,72,213]
[2,8,24,73,213]
[2,8,25,73,213]
[2,8,25,73,214]
[2,8,25,73,215]
[2,8,25,73,216]
[2,8,25,73,217]
[2,8,25,74,217]
[2,8,25,74,218]
[2,8,25,74,219]
[2,8,25,74,220]
[2,8,25,75,220]
[2,8,25,75,221]
[2,8,25,75,222]
[2,8,25,75,223]
[2,8,25,75,224]
[2,8,25,76,224]
[2,8,25,76,225]
[2,8,25,76,226]
[2,8,25,76,227]
[2,8,25,76,228]
[2,8,25,77,228]
[2,8,26,77,228]
[2,8,26,77,229]
[2,8,26,77,230]
[2,8,26,77,231]
[2,8,26,78,231]
[2,8,26,78,232]
[2,8,26,78,233]
[2,8,26,78,234]
[2,8,26,78,235]
[2,8,26,79,235]
[2,8,26,79,236]
[2,8,26,79,237]
[2,8,26,79,238]
[2,8,26,79,239]
[2,8,26,80,239]
[2,8,26,80,240]
[2,8,26,80,241]
[2,8,26,80,242]

-}
