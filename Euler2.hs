module Euler2 where

import qualified Euler101
import qualified Euler102
import qualified Euler103
import qualified Euler104
import qualified Euler105
import qualified Euler106
import qualified Euler107
import qualified Euler108
import qualified Euler109
import qualified Euler110
import qualified Euler111
import qualified Euler112
import qualified Euler113
import qualified Euler114
import qualified Euler115
import qualified Euler116
import qualified Euler117
import qualified Euler118
import qualified Euler119
import qualified Euler120
import qualified Euler121
import qualified Euler122
import qualified Euler123
import qualified Euler124
import qualified Euler125
import qualified Euler126
import qualified Euler127
import qualified Euler128
import qualified Euler129
import qualified Euler130
import qualified Euler131
import qualified Euler132
import qualified Euler133
import qualified Euler134
import qualified Euler135
import qualified Euler136
import qualified Euler137
import qualified Euler138
import qualified Euler139
import qualified Euler140
import qualified Euler141
import qualified Euler142
import qualified Euler143
import qualified Euler144
import qualified Euler145
import qualified Euler146
import qualified Euler147
import qualified Euler148
import qualified Euler149
import qualified Euler150
import qualified Euler151
import qualified Euler152
import qualified Euler153
import qualified Euler154
import qualified Euler155
import qualified Euler156
import qualified Euler157
import qualified Euler158
import qualified Euler159
import qualified Euler160
import qualified Euler161
import qualified Euler162
import qualified Euler163
import qualified Euler164
import qualified Euler165
import qualified Euler166
import qualified Euler167
import qualified Euler168
import qualified Euler169
import qualified Euler170
import qualified Euler171
import qualified Euler172
import qualified Euler173
import qualified Euler174
import qualified Euler175
import qualified Euler176
import qualified Euler177
import qualified Euler178
import qualified Euler179
import qualified Euler180
import qualified Euler181
import qualified Euler182
import qualified Euler183
import qualified Euler184
import qualified Euler185
import qualified Euler186
import qualified Euler187
import qualified Euler188
import qualified Euler189
import qualified Euler190
import qualified Euler191
import qualified Euler192
import qualified Euler193
import qualified Euler194
import qualified Euler195
import qualified Euler196
import qualified Euler197
import qualified Euler198
import qualified Euler199
import qualified Euler200

checks :: [(Int, Double, Int, IO String, String, String)]
checks = [
  (101, 0.0,  1, Euler101.main, "37076114526", "Optimum Polynomial Function"),
  (102, 0.2,  1, Euler102.main, "228", "Triangles Containing the Origin"),
  (103, 0.7,  2, Euler103.main, "20313839404245", "Special Subset Sum Property"),
  (104, 0.8,  1, Euler104.main, "329468", "Pandigital Fibonacci Numbers"),
  (105, 0.2,  1, Euler105.main, "73702", "Sum of Special Sum Sets"),
  (106, 0.0,  1, Euler106.main, "21384", "Identifying Special Sum Sets"),
  (107, 0.0,  1, Euler107.main, "259679", "Connecting the Network"),
  (108, 0.0,  1, Euler108.main, "180180", "Solving 1/x + 1/y = 1/n"),
  (109, 0.0,  1, Euler109.main, "38182", "Ways to Checkout in Darts"),
  (110, 0.4,  1, Euler110.main, "9350130049860600", "Counting Solutions of 1/x + 1/y = 1/n"),
  (111, 0.5,  2, Euler111.main, "612407567715", "Primes with Repeated Digits"),
  (112, 0.8,  1, Euler112.main, "1587000", "Density of Bouncy Numbers"),
  (113, 0.0,  1, Euler113.main, "51161058134250", "Counting Bouncy Numbers"),
  (114, 0.0,  1, Euler114.main, "16475640049", "Rows of Separated Blocks I"),
  (115, 0.0,  1, Euler115.main, "168", "Rows of Separated Blocks II"),
  (116, 0.0,  1, Euler116.main, "20492570929", "Three Coloured Tiles"),
  (117, 0.0,  1, Euler117.main, "100808458960497", "Different Sized Tiles"),
  (118, 3.7,  2, Euler118.main, "44680", "Sets of Primes Using All Digits"),
  (119, 0.1,  1, Euler119.main, "248155780267521", "Sum of Digits Raised to Powers"),
  (120, 0.0,  1, Euler120.main, "333082500", "Find the Maximum Remainder"),
  (121, 0.0,  1, Euler121.main, "2269", "Game of Chance with Coloured Discs"),
  (122, 1.6,  1, Euler122.main, "1582", "Most Efficient Exponentiation Method"),
  (123, 0.2,  3, Euler123.main, "21035", "Determine the Remainder"),
  (124, 1.6, 18, Euler124.main, "21417", "Sorted Radical Function"),
  (125, 4.5,  2, Euler125.main, "2906969179", "Square sums that are palindromic"),
  (126, 1.3,  2, Euler126.main, "18522", "Cubes Required to Cover a Cuboid"),
  (127, 3.0, 13, Euler127.main, "15377700", "Counting abc-hits"),
  (128, 0.2,  1, Euler128.main, "14516824220", "Prime Differences of Hexagonal Tiles"),
  (129, 0.0,  1, Euler129.main, "1000023", "Minimal Repunits that Divide by n"),
  (130, 0.3,  1, Euler130.main, "149253", "Repunit Divisibility"),
  (131, 0.0,  1, Euler131.main, "173", "Primes where n^3 + n^2 p is a Cube"),
  (132, 0.1,  2, Euler132.main, "843296", "Factoring a Very Large Repunit"),
  (133, 0.1,  2, Euler133.main, "453647705", "Primes Never Dividing Repunit(10^n)"),
  (134, 0.7,  5, Euler134.main, "18613426663617118", "Pair of Consecutive Primes"),
  (135, 0.7,  5, Euler135.main, Euler135.answer, "Counting Solutions to x^2 - y^2 - z^2 = n"),
  (136, 2.6,  4, Euler136.main, "2544559", "Unique Solutions to x^2 - y^2 - z^2 = n"),
  (137, 0.0,  1, Euler137.main, "1120149658760", "Infinite Series with Fibonacci Coefficients"),
  (138, 0.0,  1, Euler138.main, "1118049290473932", "Isosceles Triangles with Height ~ Base"),
  (139, 0.0,  1, Euler139.main, "10057761", "Tiling Pythagorean Triangles"),
  (140, 0.0,  1, Euler140.main, "5673835352990", "Infinite Series with Linear-Recurrent Coefficients"),
  (141,  13, 23, Euler141.main, Euler141.answer, "Progressive perfect squares"),
  (142, 6.0,  2, Euler142.main, "1006193", "Perfect Square Collection"),
  (143, 0.9, 16, Euler143.main, "25587759", "Torricelli point of a triangle"),
  (144, 0.0,  1, Euler144.main, "354", "Reflections of a Laser Beam"),
  (145, 0.0,  1, Euler145.main, "608720", "Counting Reversible Numbers"),
  (146, 3.0,  2, Euler146.main, "676333270", "Investigating a Prime Pattern"),
  (147, 0.0,  1, Euler147.main, "846910284", "Rectangles in cross-hatched grids"),
  (148, 0.0,  1, Euler148.main, "2129970655314432", "Exploring Pascal's triangle."),
  (149, 8.0,190, Euler149.main, Euler149.answer, "Searching for a maximum-sum subsequence"),
  (150,  33,  7, Euler150.main, Euler150.answer, "Minimum-sum sub-triangles"),
  (151, 0.0,  1, Euler151.main, "0.464398781601087", "Paper sheets of standard sizes"),
  (152,  14,  2, Euler152.main, Euler152.answer, "Writing 1/2 as a sum of inverse squares"),
  (153,  22,  3, Euler153.main, Euler153.answer, "Investigating Gaussian Integers"),
  (154, 118,  4, Euler154.main, "479742450", "Exploring Pascal's pyramid"),
  (155,  12, 71, Euler155.main, Euler155.answer, "Counting Capacitor Circuits"),
  (156, 1.3,  1, Euler156.main, "21295121502550", "Counting Digits"),
  (157, 0.1,  1, Euler157.main, "53490", "Solving 1/a + 1/b = p/10^n"),
  (158, 0.0,  1, Euler158.main, "409511334375", "One Character Out of Order"),
  (159, 4.6, 34, Euler159.main, "14489159", "Digital root sums of factorizations."),
  (160, 0.9,  4, Euler160.main, "16576", "Factorial trailing digits"),
  (161, 1.2, 44, Euler161.main, "20574308184277971", "Triominoes"),
  (162, 0.0,  1, Euler162.main, "3D58725572C62302", "Hexadecimal numbers"),
  (163, 0.0,  1, Euler163.main, "343047", "Cross-hatched triangles"),
  (164, 0.0,  1, Euler164.main, "378158756814587", "No 3 Consecutive Large Digits"),
  (165,  71,  9, Euler165.main, Euler165.answer, "Intersections"),
  (166, 0.8,  2, Euler166.main, "7130034", "Criss Cross"),
  (167, 0.4,  1, Euler167.main, "3916160068885", "Investigating Ulam sequences"),
  (168, 0.0,  1, Euler168.main, "59206", "Number Rotations"),
  (169, 0.0,  1, Euler169.main, "178653872807", "Numbers as Sums of Powers of 2"),
  (170, 0.5,  2, Euler170.main, "9857164023", "Pandigital Concatenated Products"),
  (171, 0.2,  3, Euler171.main, "142989277", "Sum of Squares of Digits is a Square"),
  (172, 0.0,  1, Euler172.main, "227485267000992000", "Numbers with Few Repeated Digits"),
  (173, 0.0,  1, Euler173.main, "1572729", "Counting Hollow Square Laminae"),
  (174, 0.2,  2, Euler174.main, "209566", "Arrangements of Hollow Square Laminae"),
  (175, 0.0,  1, Euler175.main, "1,13717420,8", "Numbers as Sums of Powers of 2"),
  (176, 0.0,  1, Euler176.main, "96818198400000", "Right Triangles Sharing a Cathetus"),
  (177, 3.2,  2, Euler177.main, "129325", "Integer Angled Quadrilaterals"),
  (178, 0.0,  1, Euler178.main, "126461847755", "Step Numbers"),
  (179, 7.8, 80, Euler179.main, "986262", "Consecutive positive divisors"),
  (180, 1.0,  2, Euler180.main, "285196020571078987", "Rational Zeros of a Function"),
  (181, 1.0, 68, Euler181.main, "83735848679360680", "Grouping objects of two different colours"),
  (182, 1.0,  1, Euler182.main, "399788195976", "RSA Encryption"),
  (183, 0.1,  1, Euler183.main, "48861552", "Maximum Product of Parts"),
  (184, 0.1,  1, Euler184.main, "1725323624056", "Triangles Containing the Origin"),
  (185,  16,  2, Euler185.main, "4640261571849533", "Number Mind"),
  (186, 4.4,113, Euler186.main, "2325629", "Connectedness of a Network"),
  (187, 6.0,108, Euler187.main, "17427258", "Semiprimes"),
  (188, 0.0,  1, Euler188.main, "95962097", "Hyperexponentiation"),
  (189, 0.8, 21, Euler189.main, "10834893628237824", "Tri-colouring a triangular grid"),
  (190, 0.0,  1, Euler190.main, "371048281", "Maximizing a Weighted Product"),
  (191, 0.0,  1, Euler191.main, "1918080160", "Prize Strings"),
  (192, 2.4,  2, Euler192.main, "57060635927998347", "Best Approximations"),
  (193,  16, 28, Euler193.main, "684465067343069", "Squarefree Numbers"),
  (194, 0.0,  1, Euler194.main, "61190912", "Coloured Configurations"),
  (195, 4.8, 77, Euler195.main, "75085391", "Inscribed Circles of Triangles"),
  (196,  42,  9, Euler196.main, Euler196.answer, "Prime triplets"),
  (197, 0.0,  1, Euler197.main, "1.710637717", "A Recursively Defined Sequence"),
  (198, 0.5,  2, Euler198.main, "52374425", "Ambiguous Numbers"),
  (199, 0.1,  1, Euler199.main, "0.00396087", "Iterative Circle Packing"),
  (200, 0.9,  3, Euler200.main, "229161792008", "200th prime-proof sqube")
  ]
