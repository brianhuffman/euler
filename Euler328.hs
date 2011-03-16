module Euler328 where
import EulerLib
import Data.Array

{-

Problem 328
12 March 2011

We are trying to find a hidden number selected from the set of
integers {1, 2, ..., n} by asking questions. Each number (question) we
ask, has a cost equal to the number asked and we get one of three
possible answers:

    * "Your guess is lower than the hidden number", or
    * "Yes, that's it!", or
    * "Your guess is higher than the hidden number".

Given the value of n, an optimal strategy minimizes the total cost
(i.e. the sum of all the questions asked) for the worst possible case.

E.g. if n=3, the best we can do is obviously to ask the number "2".
The answer will immediately lead us to find the hidden number (at a
total cost = 2).

If n=8, we might decide to use a "binary search" type of strategy: Our
first question would be "4" and if the hidden number is higher than 4
we will need one or two additional questions. Let our second question
be "6". If the hidden number is still higher than 6, we will need a
third question in order to discriminate between 7 and 8. Thus, our
third question will be "7" and the total cost for this worst-case
senario will be 4+6+7=17.

We can improve considerably the worst-case cost for n=8, by asking "5"
as our first question. If we are told that the hidden number is higher
than 5, our second question will be "7", then we'll know for certain
what the hidden number is (for a total cost of 5+7=12). If we are told
that the hidden number is lower than 5, our second question will be
"3" and if the hidden number is lower than 3 our third question will
be "1", giving a total cost of 5+3+1=9. Since 12>9, the worst-case
cost for this strategy is 12. That's better than what we achieved
previously with the "binary search" strategy; it is also better than
or equal to any other strategy. So, in fact, we have just described an
optimal strategy for n=8.

Let C(n) be the worst-case cost achieved by an optimal strategy for n,
as described above. Thus C(1) = 0, C(2) = 1, C(3) = 2 and C(8) = 12.
Similarly, C(100) = 400 and SUM n=[1..100] C(n) = 17575.

Find SUM n=[1..200000] C(n).

-}

{-
======================================================================
brute force solution
======================================================================
-}

costs :: Int -> Array Int (Array Int Int)
costs nmax = a
  where
    a = funArray (1, nmax) f
    f n = funArray (1, n) (g n)
    g n m
      | m == n    = 0
      | m+1 == n  = m
      | m+2 == n  = m+1
      | otherwise =
          minimum [ k + max (a!(k-1)!m) (a!n!(k+1)) | k <- [m+1 .. n-1] ]

prob328a nmax = sum [ toInteger (a!n!1) | n <- [1..nmax] ]
  where a = costs nmax

{-
======================================================================
Solution 2: Finding piecewise-linear patterns
======================================================================

C(n+1,n+1) = 0
C(n+1,n+2) = n+1
C(n+1,n+3) = n+2
C(n+1,n+4) = 2n+4
C(n+1,n+5) = 2n+6
C(n+1,n+6) = 2n+8
C(n+1,n+7) = 2n+10
C(n+1,n+8) = 2n+12 (n=0..3)
           = 3n+9 (n=4..)
C(n+1,n+9) = 2n+14 (n=0..2)
           = 3n+12 (n=3..)
...

-}


type Seq = (Int, [(Int, Int)])

-- precondition: 0 < k <= sum (map snd ds)
shiftSeq :: Int -> Seq -> Seq
shiftSeq k (n, (d, r) : ds) =
  case compare k r of
    LT -> (n+k*d, (d, r-k) : ds)
    EQ -> (n+k*d, ds)
    GT -> shiftSeq (k-r) (n+r*d, ds)

cons :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
cons (d1, 0) ds = ds
cons (d1, r1) ((d2, r2) : ds)
  | d1 == d2 = (d2, r1+r2) : ds
cons x xs = x : xs

dropR :: Int -> [(Int, Int)] -> [(Int, Int)]
dropR k ((d, r) : ds) =
  case compare k r of
    LT -> (d, r-k) : ds
    EQ -> ds
    GT -> dropR (k-r) ds

maxSeq :: Seq -> Seq -> Seq
maxSeq (n1, ds1) (n2, ds2)
  | n1 <= n2  = (n2, maxR (n2-n1) ds1 ds2)
  | otherwise = (n1, maxR (n1-n2) ds2 ds1)

-- precondition: z >= 0
maxR z ds1@((d1, r1) : ds1') ds2@((d2, r2) : ds2')
  | r1 < r2 = maxR z ds1 ((d2, r1) : (d2, r2-r1) : ds2')
  | r2 < r1 = maxR z ((d1, r2) : (d1, r1-r2) : ds1') ds2
  | 0 <= zr = cons (d2, r2) (maxR zr ds1' ds2')
  | d1 > d2+z = cons (d1-z, 1) (maxR (d1-d2-z) (dropR 1 ds2) (dropR 1 ds1))
  | otherwise = cons (d2, r') (maxR z' (dropR r' ds1) (dropR r' ds2))
  where
    zr = z + r1*(d2-d1)
    (r', z')  = z `divMod` (d1-d2)
maxR z ds1 ds2 = []

minSeq :: Seq -> Seq -> Seq
minSeq (n1, ds1) (n2, ds2)
  | n1 <= n2  = (n1, minR (n2-n1) ds1 ds2)
  | otherwise = (n2, minR (n1-n2) ds2 ds1)

-- precondition: z >= 0
minR z ds1@((d1, r1) : ds1') ds2@((d2, r2) : ds2')
  | r1 < r2 = minR z ds1 ((d2, r1) : (d2, r2-r1) : ds2')
  | r2 < r1 = minR z ((d1, r2) : (d1, r1-r2) : ds1') ds2
  | 0 <= zr = cons (d1, r1) (minR zr ds1' ds2')
  | d1 > d2+z = cons (d2+z, 1) (minR (d1-d2-z) (dropR 1 ds2) (dropR 1 ds1))
  | otherwise = cons (d1, r') (minR z' (dropR r' ds1) (dropR r' ds2))
  where
    zr = z + r1*(d2-d1)
    (r', z')  = z `divMod` (d1-d2)
minR z ds1 ds2 = []

incSeq :: Int -> Seq -> Seq
incSeq k (n, ds) = (n+k, map (\(d, r) -> (d+1, r)) ds)

mkSeq :: [Int] -> Seq
mkSeq xs = (head xs, foldr cons [] (map (\d -> (d, 1)) (deltas xs)))

unSeq :: Seq -> [Int]
unSeq (n, ds) = scanl (+) n (concatMap (\(d,r) -> replicate r d) ds)

test nmax =
  [ (n, mkSeq xs) |
    n <- [1..nmax],
    let xs = [ a!(k+n)!(k+1) | k <- [0..nmax-n] ] ]
  where a = costs nmax
-- test nmax = assocs (arr328b nmax)

-- arr328b!n = [(m,b,i,j)] means C[x+1..x+n] = m*x+b for x = {i..j}
arr328b :: Int -> Array Int Seq
arr328b nmax = a
  where
    a = funArray (1, nmax) f
    f 1 = (0, [(0, nmax-1)])
    f 2 = (1, [(1, nmax-2)])
    f n =
      foldr1 minSeq
        [ incSeq k (maxSeq seq1 seq2) | 
          k <- [(n+1)`div`2..n-1],
          let seq1 = a!(k-1),
          let seq2 = shiftSeq k (a!(n-k))
        ]

-- mapM_ print $ zip [0..] $ scanl (+) 0 $ map (toInteger . fst) $ Data.Array.elems $ arr328b 200000

prob328b = sum . map (toInteger . fst) . elems . arr328b


{-
======================================================================
Solution 3: Encoding runs of identical scores
======================================================================

Let C(m,n) = worst-case cost for identifying a number from {m..n}

Table of C(m,n)

   |  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
-------------------------------------------------------------------------------
 1 |  0
 2 |  1  0
 3 |  2  2  0
 4 |  4  3  3  0
 5 |  6  6  4  4  0
 6 |  8  8  8  5  5  0
 7 | 10 10 10 10  6  6  0
 8 | 12 12 12 12 12  7  7  0
 9 | 14 14 14 14 14 14  8  8  0
10 | 16 16 16 16 16 16 16  9  9  0
11 | 18 18 18 18 18 18 18 18 10 10  0
12 | 21 21 21 21 21 20 20 20 20 11 11  0
13 | 24 24 24 24 24 24 22 22 22 22 12 12  0
14 | 27 27 27 27 27 27 27 24 24 24 24 13 13  0
15 | 30 30 30 30 30 30 30 30 26 26 26 26 14 14  0
16 | 34 34 34 34 34 33 33 33 33 28 28 28 28 15 15  0
17 | 38 38 38 38 38 38 36 36 36 36 30 30 30 30 16 16  0
18 | 42 42 42 42 42 42 42 39 39 39 39 32 32 32 32 17 17  0
19 | 46 46 46 46 46 46 46 46 42 42 42 42 34 34 34 34 18 18  0
20 | 49 49 49 49 49 49 49 49 49 45 45 45 45 36 36 36 36 19 19  0
21 | 52 52 52 52 52 52 52 52 52 52 48 48 48 48 38 38 38 38 20 20  0
22 | 55 55 55 55 55 55 55 55 55 55 55 51 51 51 51 40 40 40 40 21 21  0
23 | 58 58 58 58 58 58 58 58 58 58 58 58 54 54 54 54 42 42 42 42 22 22  0
24 | 61 61 61 61 61 61 61 61 61 61 61 61 61 57 57 57 57 44 44 44 44 23 23  0
25 | 64 64 64 64 64 64 64 64 64 64 64 64 64 64 60 60 60 60 46 46 46 46 24 24  0
-------------------------------------------------------------------------------
   |  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25

Encode each row by minimal positions of each score. E.g. row 25:
[(25,0),(23,24),(19,46),(15,60),(1,64)]

In an optimal tree, the right subtree will always have a least element
that is as small as possible for its score. This limits the set of root
values to consider for new trees.

List of minimal m with same C(m,n) value
---------------------------------------------
 1 | ( 1,0)
 2 | ( 2,0) ( 1, 1)
 3 | ( 3,0) ( 1, 2)
 4 | ( 4,0) ( 2, 3) ( 1, 4)
 5 | ( 5,0) ( 3, 4) ( 1, 6)
 6 | ( 6,0) ( 4, 5) ( 1, 8)
 7 | ( 7,0) ( 5, 6) ( 1,10)
 8 | ( 8,0) ( 6, 7) ( 1,12)
 9 | ( 9,0) ( 7, 8) ( 1,14)
10 | (10,0) ( 8, 9) ( 1,16)
11 | (11,0) ( 9,10) ( 1,18)
{12,0}
 p=11: {10,11} (8,20) (1,27)
        p=9:  {6,20} (1,21)
               p=5:  (1,25)

12 | (12,0) (10,11) ( 6,20) ( 1,21)
13 | (13,0) (11,12) ( 7,22) ( 1,24)
14 | (14,0) (12,13) ( 8,24) ( 1,27)
15 | (15,0) (13,14) ( 9,26) ( 1,30)
16 | (16,0) (14,15) (10,28) ( 6,33) ( 1,34)
17 | (17,0) (15,16) (11,30) ( 7,36) ( 1,38)
18 | (18,0) (16,17) (12,32) ( 8,39) ( 1,42)
19 | (19,0) (17,18) (13,34) ( 9,42) ( 1,46)
20 | (20,0) (18,19) (14,36) (10,45) ( 1,49)
21 | (21,0) (19,20) (15,38) (11,48) ( 1,52)
22 | (22,0) (20,21) (16,40) (12,51) ( 1,55)
23 | {23,0}
      p=22: {21,22} (19,42) (15,59) (11,69) (1,73)
             p=20:  (19,42)-{17,42} {13,54} (9,62) (1,66)
                             p=16: (15,58)-(13,58)-(9,58)-{1,58}
                                                            end
23 | (23,0) (21,22) (17,42) (13,54) (1,58)

-}

arr328c :: Int -> Array Int [(Int, Int)]
arr328c nmax = a
  where
    a = funArray (1, nmax) f
    -- costs of trees with max n (never nil)
    f n = g [(n, 0)]
    g ((1, c) : mcs) = [(1, c)]
    g ((2, c) : mcs) = [(2, c), (1, c+1)]
    g ((m, c) : mcs) = (m, c) : g mcs'
      where mcs' = merge mcs (h (m-1) c (a!(m-2)))

merge mcs1@(mc1@(m1,c1) : mcs1') mcs2@(mc2@(m2,c2) : mcs2')
  | (c2,m2) < (c1,m1) = mcs2
  | otherwise = mc1 : merge mcs1' (dropWhile (\(m,c) -> m1 <= m) mcs2)
merge [] mcs2 = mcs2
merge mcs1 [] = mcs1

-- costs of trees with root p, cost of rhs = c0 (never nil)
h p c0 (_ : mcs@((m, c) : _)) | c <= c0 = h p c0 mcs
h p c0 ((m, c) : mcs) = (m, max c0 c + p) : map (\(m, c) -> (m, c + p)) mcs

prob328c_list = map (snd . last) . elems . arr328c

prob328c = sum . map (toInteger . snd . last) . elems . arr328c

prob328c' = zip [0,1000..] . scanl (+) 0 . map sum . chunk 1000 . map (toInteger . snd . last) . elems . arr328c

{-

122 | (122,0) (120,121) (116,240) (112,351) (108,355) (104,442) (100,446) (92,462) (88,485) (84,489) (76,505) (72,512) (68,516) (52,532) (1,543)

{124,0}
p=123: {122,123},(120,244),(116,363),(112,474),(108,478),(104,565),(100,569),(92,585),(88,608),(84,612),(76,628),(72,635),(68,639),(52,655),(1,666)
p=121: {118,244} {114,357} (110,466) (106,470) (102,555),(98,559),(90,575),(86,596),(82,600),(74,616),(70,621),(66,625),(50,641),(1,650)
p=117: {110,361} {106,450} {102,454} (98,535) (94,539) (86,555),(82,572),(78,576),(70,592),(66,593),(62,597),(46,613),(1,618)
      p=113: (98,470) (94,515) (90,519),(82,535),(78,548),(74,552),(62,568),(58,569),(42,585),(1,586)
p=109: {94,470} {90,495} {86,499} {78,515} {74,524} {70,528} {54,544} (1,557)
   p=105: {1,555}
   p=101: (1,555)
p=93: (1,563)

[(124,0),(122,123),(118,244),(114,357),(110,361),(106,450),(102,454),(94,470),(90,495),(86,499),(78,515),(74,524),(70,528),(54,544),(1,557)]

Which rows depend on which other rows:
(1,[])
(2,[])
(3,[1])
(4,[2])
(5,[3,1])
(6,[4,2])
(7,[5,3])
(8,[6,4])
(9,[7,5])
(10,[8,6])
(11,[9,7])
(12,[10,8,4])
(13,[11,9,5])
(14,[12,10,6])
(15,[13,11,7])
(16,[14,12,8,4])
(17,[15,13,9,5])
(18,[16,14,10,6])
(19,[17,15,11,7])
(20,[18,16,12,8])
(21,[19,17,13,9])
(22,[20,18,14,10])
(23,[21,19,15,11])
(24,[22,20,16,12])
(25,[23,21,17,13])
(26,[24,22,18,14])
(27,[25,23,19,15])
(28,[26,24,20,16])
(29,[27,25,21,17])
(30,[28,26,22,18])
(31,[29,27,23,19])
(32,[30,28,24,20,12])
(33,[31,29,25,21,13])
(34,[32,30,26,22,14])
(35,[33,31,27,23,15])
(36,[34,32,28,24,20,16])
(37,[35,33,29,25,21,17])
(38,[36,34,30,26,22,18])
(39,[37,35,31,27,23,19])
(40,[38,36,32,28,24,20,12])
(41,[39,37,33,29,25,21,13])
(42,[40,38,34,30,26,22,14])
(43,[41,39,35,31,27,23,15])
(44,[42,40,36,32,28,24,20,16])
(45,[43,41,37,33,29,25,21,17])
(46,[44,42,38,34,30,26,22,18])
(47,[45,43,39,35,31,27,23,19])
(48,[46,44,40,36,32,28,24,20,12])
(49,[47,45,41,37,33,29,25,21,13])
(50,[48,46,42,38,34,30,26,22,14])
(51,[49,47,43,39,35,31,27,23,15])
(52,[50,48,44,40,36,32,28,24,20,16])
(53,[51,49,45,41,37,33,29,25,21])
(54,[52,50,46,42,38,34,30,26,22])
(55,[53,51,47,43,39,35,31,27])
(56,[54,52,48,44,40,36,32,28])
(57,[55,53,49,45,41,37,33,29])
(58,[56,54,50,46,42,38,34,30])
(59,[57,55,51,47,43,39,35])
(60,[58,56,52,48,44,40,36])
(61,[59,57,53,49,45,41,37])
(62,[60,58,54,50,46,42,38])
(63,[61,59,55,51,47,43,39])
(64,[62,60,56,52,48,44,40])
(65,[63,61,57,53,49,45,41])
(66,[64,62,58,54,50,46,42])
(67,[65,63,59,55,51,47,43])
(68,[66,64,60,56,52,48,44])
(69,[67,65,61,57,53,49,45])
(70,[68,66,62,58,54,50,46])
(71,[69,67,63,59,55,51,47])
(72,[70,68,64,60,56,52,48])
(73,[71,69,65,61,57,53,49])
(74,[72,70,66,62,58,54,50])
(75,[73,71,67,63,59,55,51])
(76,[74,72,68,64,60,56,52])
(77,[75,73,69,65,61,57,53])
(78,[76,74,70,66,62,58,54])
(79,[77,75,71,67,63,59,55])
(80,[78,76,72,68,64,60,56,40])
(81,[79,77,73,69,65,61,57,41])
(82,[80,78,74,70,66,62,58,42])
(83,[81,79,75,71,67,63,59,43])
(84,[82,80,76,72,68,64,60,44])
(85,[83,81,77,73,69,65,61,45])
(86,[84,82,78,74,70,66,62,46])
(87,[85,83,79,75,71,67,63,47])
(88,[86,84,80,76,72,68,64,48])
(89,[87,85,81,77,73,69,65,49])
(90,[88,86,82,78,74,70,66,50])
(91,[89,87,83,79,75,71,67,51])
(92,[90,88,84,80,76,72,68,52])
(93,[91,89,85,81,77,73,69,53])
(94,[92,90,86,82,78,74,70,54])
(95,[93,91,87,83,79,75,71,55])
(96,[94,92,88,84,80,76,72,60,56,40])
(97,[95,93,89,85,81,77,73,61,57,41])
(98,[96,94,90,86,82,78,74,62,58,42])
(99,[97,95,91,87,83,79,75,63,59,43])
(100,[98,96,92,88,84,80,76,68,64,60,44])
(101,[99,97,93,89,85,81,77,69,65,61,45])
(102,[100,98,94,90,86,82,78,70,66,62,46])
(103,[101,99,95,91,87,83,79,71,67,63,47])
(104,[102,100,96,92,88,84,80,72,68,64,48])
(105,[103,101,97,93,89,85,81,73,69,65,49])
(106,[104,102,98,94,90,86,82,74,70,66,50])
(107,[105,103,99,95,91,87,83,75,71,67,51])
(108,[106,104,100,96,92,88,84,76,72,68,52])
(109,[107,105,101,97,93,89,85,77,73,69,53])
(110,[108,106,102,98,94,90,86,78,74,70,54])
(111,[109,107,103,99,95,91,87,79,75,71,55])
(112,[110,108,104,100,96,92,88,80,76,72,60,56,40])
(113,[111,109,105,101,97,93,89,81,77,73,61,57,41])
(114,[112,110,106,102,98,94,90,82,78,74,62,58,42])
(115,[113,111,107,103,99,95,91,83,79,75,63,59,43])
(116,[114,112,108,104,100,96,92,84,80,76,68,64,60,44])
(117,[115,113,109,105,101,97,93,85,81,77,69,65,61,45])
(118,[116,114,110,106,102,98,94,86,82,78,70,66,62,46])
(119,[117,115,111,107,103,99,95,87,83,79,71,67,63,47])
(120,[118,116,112,108,104,100,96,88,84,80,72,68,64,48])
(121,[119,117,113,109,105,101,97,89,85,81,73,69,65,49])
(122,[120,118,114,110,106,102,98,90,86,82,74,70,66,50])
(123,[121,119,115,111,107,103,99,91,87,83,75,71,67,51])
(124,[122,120,116,112,108,104,100,92,88,84,76,72,68,52])
(125,[123,121,117,113,109,105,101,93,89,85,77,73,69,53])
(126,[124,122,118,114,110,106,102,94,90,86,78,74,70,54])
(127,[125,123,119,115,111,107,103,95,91,87,79,75,71,55])
(128,[126,124,120,116,112,108,104,96,92,88,80,76,72,60,56])
(129,[127,125,121,117,113,109,105,97,93,89,81,77,73,61,57])
(130,[128,126,122,118,114,110,106,98,94,90,82,78,74,62,58])
(131,[129,127,123,119,115,111,107,99,95,91,83,79,75,63])
(132,[130,128,124,120,116,112,108,100,96,92,84,80,76,68,64])
(133,[131,129,125,121,117,113,109,101,97,93,85,81,77,69])
(134,[132,130,126,122,118,114,110,102,98,94,86,82,78,70])
(135,[133,131,127,123,119,115,111,103,99,95,87,83,79])
(136,[134,132,128,124,120,116,112,104,100,96,88,84,80])
(137,[135,133,129,125,121,117,113,105,101,97,89,85,81])
(138,[136,134,130,126,122,118,114,106,102,98,90,86,82])
(139,[137,135,131,127,123,119,115,107,103,99,91,87,83])
(140,[138,136,132,128,124,120,116,108,104,100,92,88,84])
(141,[139,137,133,129,125,121,117,109,105,101,93,89,85])
(142,[140,138,134,130,126,122,118,110,106,102,94,90,86])
(143,[141,139,135,131,127,123,119,111,107,103,95,91,87])
(144,[142,140,136,132,128,124,120,112,108,104,96,92,88])
(145,[143,141,137,133,129,125,121,113,109,105,97,93,89])
(146,[144,142,138,134,130,126,122,114,110,106,98,94,90,50])
(147,[145,143,139,135,131,127,123,115,111,107,99,95,91,51])
(148,[146,144,140,136,132,128,124,116,112,108,100,96,92,52])
(149,[147,145,141,137,133,129,125,117,113,109,101,97,93,53])
(150,[148,146,142,138,134,130,126,118,114,110,102,98,94,54])





-}


{-
======================================================================
Solution 4: Encoding differences in successive rows
======================================================================
Doesn't save any memory!
-}

type CodedRow = Either [(Int, Int, Int)] Int
-- Left [(minimal m, score, score increment)]
-- Right (number of rows above to extrapolate from)

arr328d :: Int -> Array Int CodedRow
arr328d nmax = a
  where
    a = funArray (1, nmax) f'
    f' 1 = Left [(1,0,0)]
    f' n = putrow a n (f n)
    -- costs of trees with max n (never nil)
    f n = g [(n, 0)]
    g ((1, c) : mcs) = [(1, c)]
    g ((2, c) : mcs) = [(2, c), (1, c+1)]
    g ((m, c) : mcs) = (m, c) : g mcs'
      where mcs' = merge mcs (h (m-1) c (getrow a (m-2)))

getrow :: Array Int CodedRow -> Int -> [(Int, Int)]
getrow a n =
  case a!n of
    Left mcds -> map (\(m,c,d) -> (m,c)) mcds
    Right i -> let Left mcds = a!(n-i) in f i mcds
  where
    f i [(1,c,d)] = [(1,c+d*i)]
    f i ((m,c,d):mcds) = (m+i, c+d*i) : f i mcds

putrow :: Array Int CodedRow -> Int -> [(Int, Int)] -> CodedRow
putrow a n mcs =
    if new_mcds == predicted then Right skip else Left new_mcds
  where
    prev = a!(n-1)
    skip =
      case prev of
        Left _ -> 1
        Right i -> i+1
    predicted =
      case prev of
        Left mcds -> f 1 mcds
        Right i -> let Left mcds = a!(n-1-i) in f (i+1) mcds
    f i [(1,c,d)] = [(1, c+d*i, d)]
    f i ((m,c,d):mcds) = (m+i, c+d*i, d) : f i mcds
    new_mcds = new predicted mcs
    new ((m,c,d):mcds) ((m',c'):mcs)
      | m == m' && c == c' = (m,c,d) : new mcds mcs
      | m < m' = (m',c',d+c'-c) : new ((m,c,d):mcds) mcs
      | otherwise = (m',c',d+c'-c) : new mcds mcs
    new _ _ = []

getcost :: Array Int CodedRow -> Int -> Int
getcost a n =
  case a!n of
    Left mcds -> case last mcds of (m,c,d) -> c
    Right i -> let Left mcds = a!(n-i)
               in case last mcds of (m,c,d) -> c+d*i

prob328d n = sum $ map (toInteger . getcost arr) [1..n]
  where arr = arr328d n

prob328d' n = zip [0,1000..] . scanl (+) 0 . map sum . chunk 1000 . map (toInteger . getcost arr) $ [1..n]
  where arr = arr328d n


{-
======================================================================
Solution 5: Compute the even rows and odd rows separately.
======================================================================
Observation: Values in an even row only depend on other even rows.
Similarly, odd rows depend only on other odd rows.
-}

arr328even :: Int -> Array Int [(Int, Int)]
arr328even nmax = a
  where
    a = funArray (1, nmax`div`2) (\n -> f (2*n))
    -- costs of trees with max n (never nil)
    f n = g [(n, 0)]
    g ((1, c) : mcs) = [(1, c)]
    g ((2, c) : mcs) = [(2, c), (1, c+1)]
    g ((m, c) : mcs) = (m, c) : g mcs'
      where mcs' = merge mcs (h (m-1) c (a!((m-2)`div`2)))

arr328odd :: Int -> Array Int [(Int, Int)]
arr328odd nmax = a
  where
    a = funArray (1, nmax`div`2) (\n -> f (2*n-1))
    -- costs of trees with max n (never nil)
    f n = g [(n, 0)]
    g ((1, c) : mcs) = [(1, c)]
    g ((2, c) : mcs) = [(2, c), (1, c+1)]
    g ((m, c) : mcs) = (m, c) : g mcs'
      where mcs' = merge mcs (h (m-1) c (a!((m-1)`div`2)))

prob328even_list = map (snd . last) . elems . arr328even

prob328even = sum . map (toInteger . snd . last) . elems . arr328even

prob328even' = zip [0,1000..] . scanl (+) 0 . map sum . chunk 500 . map (toInteger . snd . last) . elems . arr328even

prob328odd_list = map (snd . last) . elems . arr328odd

prob328odd = sum . map (toInteger . snd . last) . elems . arr328odd

prob328odd' = zip [0,1000..] . scanl (+) 0 . map sum . chunk 500 . map (toInteger . snd . last) . elems . arr328odd

{-
======================================================================
Solution 5: Compute the rows separately by mod 4.
======================================================================
-}

arr328mod4 :: Int -> Int -> Array Int [(Int, Int)]
arr328mod4 r nmax = a
  where
    a = funArray (1, nmax`div`4) (\n -> f (4*n-r))
    -- costs of trees with max n (never nil)
    f 1 = [(1,0)]
    f 2 = [(2,0),(1,1)]
    f 3 = [(3,0),(1,2)]
    f 4 = [(4,0),(2,3),(1,4)]
    f n = g [(n-2, n-1)]
    g ((1, c) : mcs) = [(1, c)]
    g ((m, c) : mcs) = (m, c) : g mcs'
      where mcs' = merge mcs (h (m-1) c (a!((m-2+r)`div`4)))

prob328mod4_list r = map (snd . last) . elems . arr328mod4 r

prob328mod4 r = sum . map (toInteger . snd . last) . elems . arr328mod4 r

prob328mod4' r = zip [0,1000..] . scanl (+) 0 . map sum . chunk 250 . map (toInteger . snd . last) . elems . arr328mod4 r

prob328e n = sum [ prob328mod4 r n | r <- [0..3] ]

{-
======================================================================
Experiment
======================================================================

Let D(m,k) = optimal tree for {m-k+1 .. m} (k elements)
D(m,k) is monotone in both arguments.


Where do new tree shapes become optimal?
D( 1, 1): cost  0, depth 0
D( 2, 2): cost  1, depth 1
D( 4, 4): cost  4, depth 2
D(12, 8): cost 21, depth 3
D(16,12): cost 34, depth 4
D(20,12): cost 49, depth 3
D(32,20): cost 86, depth 4
D(40,28): cost 119, depth 5
D(48,36): cost 160, depth 6
-}

guess_d :: Int -> Int -> Int
guess_d m k = sum (map f xs)
  where
    f (m0, k0, d) = if m0 <= m && k0 <= k then d*(m-m0+1) else 0
    xs = [
     (2,2,1),
     (4,4,1),
     (12,8,1),
     (16,12,1), (20,12,-1),
     (32,20,1),
     (36,16,1), (36,20,-1),
     (40,28,1),
     (44,24,1), (44,28,-1),
     (48,36,1),
     (52,32,1), (52,36,-3),
     (54,32,-2), (54,36,2),
     (56,28,-1), (56,32,1),
     (60,24,-1), (60,28,1),
     (80,40,1),
     (96,36,1), (96,40,-1), (96,56,1),
     (100,32,1), (100,36,-1),
     (112,52,1), (112,56,-1), (112,72,1),
     (116,48,1), (116,52,-1),
     (124,72,-2),
     (128,68,1), (128,72,-1),
     (132,64,1), (132,68,-3), (132,72,2),
     (134,64,-2), (134,68,2),
     (136,56,-1), (136,64,1)
     ]

d_array :: Int -> Array Int (Array Int Int)
d_array nmax = funArray (1, nmax) f
  where
    c = costs nmax
    f m = funArray (1, m) (d m)
    d m k = c!m!(m-k+1)

d_array' :: Int -> Array Int (Array Int Int)
d_array' nmax = funArray (1, nmax) f
  where
    c = costs nmax
    f m = funArray (1, m) (d m)
    d m k = c!m!(m-k+1) - guess_d m k

least_nonzero :: Int -> (Int, Int, Int)
least_nonzero nmax = head
    [ (m, k, x) |
      m <- [1..nmax],
      let arr_m = arr!m,
      k <- [1..m],
      let x = arr_m!k,
      x /= 0 ]
  where
    arr = d_array' nmax

{-
 D | 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
-------------------------------------------------------------------------------
 1 | 0
 2 | 0  1
 3 | 0  2  2
 4 | 0  3  3  4
 5 | 0  4  4  6  6
 6 | 0  5  5  8  8  8
 7 | 0  6  6 10 10 10 10
 8 | 0  7  7 12 12 12 12 12
 9 | 0  8  8 14 14 14 14 14 14
10 | 0  9  9 16 16 16 16 16 16 16
11 | 0 10 10 18 18 18 18 18 18 18 18
12 | 0 11 11 20 20 20 20 21 21 21 21 21
13 | 0 12 12 22 22 22 22 24 24 24 24 24 24
14 | 0 13 13 24 24 24 24 27 27 27 27 27 27 27
15 | 0 14 14 26 26 26 26 30 30 30 30 30 30 30 30
16 | 0 15 15 28 28 28 28 33 33 33 33 34 34 34 34 34
17 | 0 16 16 30 30 30 30 36 36 36 36 38 38 38 38 38 38
18 | 0 17 17 32 32 32 32 39 39 39 39 42 42 42 42 42 42 42
19 | 0 18 18 34 34 34 34 42 42 42 42 46 46 46 46 46 46 46 46
20 | 0 19 19 36 36 36 36 45 45 45 45 49 49 49 49 49 49 49 49 49
21 | 0 20 20 38 38 38 38 48 48 48 48 52 52 52 52 52 52 52 52 52 52
22 | 0 21 21 40 40 40 40 51 51 51 51 55 55 55 55 55 55 55 55 55 55 55
23 | 0 22 22 42 42 42 42 54 54 54 54 58 58 58 58 58 58 58 58 58 58 58 58
24 | 0 23 23 44 44 44 44 57 57 57 57 61 61 61 61 61 61 61 61 61 61 61 61 61
25 | 0 24 24 46 46 46 46 60 60 60 60 64 64 64 64 64 64 64 64 64 64 64 64 64 64
-------------------------------------------------------------------------------
   | 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25

-}

{-
======================================================================
Tables of pivots

   | 1      2      3      4      5      6      7      8      9
---------------------------------------------------------------
 1 | -
 2 | 1      -
 3 | 2      2      -
 4 | 3      3      3      -
 5 | 2,4    4      4      4      -
 6 | 3      3,5    5      5      5      - 
 7 | 4      4      4,6    6      6      6      -
 8 | 5      5      5      5,7    7      7      7      -
 9 | 6      6      6      6      6,8    8      8      8      -
10 | 7      7      7      7      7      7,9    9      9      9
11 | 8      8      8      8      8      8      8,10   10     10
12 | 9      9      9      9      9      9      9      9,11   11
13 | 10     10     10     10     10     10     10     10     10,12
14 | 11     11     11     11     11     11     11     11     11
15 | 12     12     12     12     12     12     12     12     12
16 | 13     13     13     13     13     13     13     13     13
17 | 14     14     14     14     14     14     14     14     14
18 | 15     15     15     15     15     15     15     15     15
19 | 12,16  12,16  12,16  12,16  12,16  12,16  12,16  12,16  16
20 | 13     13     13     13     13     13     13     13     13
-}

pivots :: Int -> Array Int (Array Int [Int])
pivots nmax = funArray (1, nmax) f
  where
    c = costs nmax
    f n = funArray (1, n) (g n)
    g n m
      | m == n    = [m]
      | m+1 == n  = [m]
      | m+2 == n  = [m+1]
      | otherwise =
          [ k | k <- [m+1 .. n-1],
                k + max (c!(k-1)!m) (c!n!(k+1)) == c!n!m ]

minima :: [(Int, a)] -> (Int, [a])
minima [] = error "minima: empty list"
minima [(n, x)] = (n, [x])
minima ((n, x) : ns) =
  case compare n m of
    LT -> (n, [x])
    EQ -> (n, x : xs)
    GT -> (m, xs)
  where (m, xs) = minima ns

cost_pivot :: Int -> Array Int (Array Int (Int, [Int]))
cost_pivot nmax = a
  where
    a = funArray (1, nmax) f
    f n = funArray (1, n) (g n)
    g n m
      | m == n    = (0, [m])
      | m+1 == n  = (m, [m])
      | m+2 == n  = (m+1, [m+1])
      | otherwise = minima
          [ (k + max (fst(a!(k-1)!m)) (fst(a!n!(k+1))), k) |
            k <- [m+1 .. n-1] ]

costs_list = map elems . elems . costs
pivots_list = map elems . elems . pivots
cp_list n = zipWith zip (costs_list n) (pivots_list n)

{-
Some optimal trees

 01
   02

  02
01  03

  __03
01    04
  02

    __04
  02    05
01  03

  02__
01    04
    03  05

      __05
  __03    06  (bad)
01    04
  02

  __03__
01      05
  02  04  06

    __04__
  02      06
01  03  05  07

      __05__
  __03      07
01    04  06  08
  02


                           ______16__
                   ______12__        18
           ______08__        14    17  19
     ____04__        10    13  15
   02        06    09  11
 01  03    05  07

                   ______12______
           ______08__          __16__
     ____04__        10      14      18
   02        06    09  11  13  14  17  19
 01  03    05  07

                     ______13______
             ______09__          __17__
       ____05__        11      15      19
   __03        07    10  12  14  16  18  20
 01    04    06  08
   02

-}

{-
                                           evens
SUM n=[1..10000]  C(n) =    456654441 =   228351587 + 
SUM n=[1..20000]  C(n) =   2004517256 =  1002312094 +
SUM n=[1..30000]  C(n) =   4728660250 =  2364414044 + 
SUM n=[1..40000]  C(n) =   8733770692 =  4367001242 + 
SUM n=[1..50000]  C(n) =  14006270244 =  7003282730 +
SUM n=[1..60000]  C(n) =  20543136710 = 10271748412 +
SUM n=[1..70000]  C(n) =  28475931340 = 14238181932 +
SUM n=[1..80000]  C(n) =  37808818012 = 18904659408 +
SUM n=[1..90000]  C(n) =  48502514159 = 24251541559 +
SUM n=[1..100000] C(n) =  60575198454 = 30287918094 +
SUM n=[1..110000] C(n) =              = 37004282231 +
SUM n=[1..120000] C(n) =              = 44395475397 +
SUM n=[1..130000] C(n) =              = 52495168431 +
SUM n=[1..140000] C(n) =              = 61363152585 +
SUM n=[1..150000] C(n) = 142006654163 = 71003827933 +
SUM n=[1..160000] C(n) =
SUM n=[1..170000] C(n) =
SUM n=[1..180000] C(n) =
SUM n=[1..190000] C(n) =
SUM n=[1..200000] C(n) =

           [4,8..]       [3,7..]       [2,6..]       [1,5..]
 50000:  3501788969 +  3501641356 +  3501493761 +  3501346158
100000: 15144277897 + 15143959032 + 15143640197 + 15143321328
150000: 35502414803 + 35501913948 + 35501413130 + 35500912282
200000: 65128989747 + 65128304976 + 65127620163 + 65126935336


                                                    111111111111111111111
       111112222233333444445555566666777778888899999000001111122222333334
   2468024680246802468024680246802468024680246802468024680246802468024680
--2+
  4 +
  8     +
 12       + -
 16                 +
 20               + -
 24                     +       -
 28                   + -     - +
 32                         +@+                     +
 36                       + #2                    + -
 40                                       +       -
 44
 48                                                         +
 52                                                       + -
 56                                               +       -           -
 60
 64                                                                 +@+
 68                                                               + #2
 72                                                       +     @ - 2
 76

-}

main :: IO String
main = return $ show $ prob328e 200000

answer :: String
answer = "260511850222"
