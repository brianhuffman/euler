module Euler3 where

import qualified Euler201
import qualified Euler202
import qualified Euler203
import qualified Euler204
import qualified Euler205
import qualified Euler206
import qualified Euler207
import qualified Euler208
import qualified Euler209
import qualified Euler210
import qualified Euler211
import qualified Euler212
import qualified Euler213
import qualified Euler214
import qualified Euler215
import qualified Euler216
import qualified Euler217
import qualified Euler218
import qualified Euler219
import qualified Euler220
import qualified Euler221
import qualified Euler222
import qualified Euler223
import qualified Euler224
import qualified Euler225
import qualified Euler226
import qualified Euler227
import qualified Euler228
import qualified Euler229
import qualified Euler230
import qualified Euler231
import qualified Euler232
import qualified Euler233
import qualified Euler234
import qualified Euler235
import qualified Euler236
import qualified Euler237
import qualified Euler238
import qualified Euler239
import qualified Euler240
import qualified Euler242
import qualified Euler244
import qualified Euler245
import qualified Euler246
import qualified Euler247
import qualified Euler248
import qualified Euler249
import qualified Euler250
import qualified Euler251
import qualified Euler252
import qualified Euler253
import qualified Euler254
import qualified Euler255
import qualified Euler256
import qualified Euler257
import qualified Euler258
import qualified Euler259
import qualified Euler260
import qualified Euler261
import qualified Euler263
import qualified Euler265
import qualified Euler266
import qualified Euler267
import qualified Euler268
import qualified Euler269
import qualified Euler270
import qualified Euler271
import qualified Euler272
import qualified Euler273
import qualified Euler274
import qualified Euler276
import qualified Euler277
import qualified Euler278
import qualified Euler284
import qualified Euler285
import qualified Euler287
import qualified Euler288
import qualified Euler290
import qualified Euler291
import qualified Euler293
import qualified Euler294
import qualified Euler297

checks :: [(Int, Double, Int, IO String, String, String)]
checks =
  [
  (201, 2.0,  6, Euler201.main, "115039000", "Subsets with a unique sum"),
  (202, 0.0,  1, Euler202.main, "1209002624", "Laserbeam"),
  (203, 0.0,  3, Euler203.main, "34029210557338", "Squarefree Binomial Coefficients"),
  (204, 0.6,  1, Euler204.main, "2944730", "Generalised Hamming Numbers"),
  (205, 0.0,  1, Euler205.main, "0.5731441", "Dice Game"),
  (206, 0.1,  1, Euler206.main, "1389019170", "Concealed Square"),
  (207, 0.0,  0, Euler207.main, "44043947822", "Integer partition equations"),
  (208, 0.6,  7, Euler208.main, Euler208.answer, "Robot Walks"),
  (209, 0.0,  1, Euler209.main, "15964587728784", "Circular Logic"),
  (210,  23,  1, Euler210.main, Euler210.answer, "Obtuse Angled Triangles"),
  (211, 2.6,  2, Euler211.main, Euler211.answer, "Divisor Square Sum"),
  (212, 1.6, 20, Euler212.main, "328968937309", "Combined Volume of Cuboids"),
  (213, 6.2, 11, Euler213.main, Euler213.answer, "Flea Circus"),
  (214, 9.1, 25, Euler214.main, Euler214.answer, "Totient Chains"),
  (215, 0.6,  5, Euler215.main, "806844323190414", "Crack-free Walls"),
  (216,  84, 25, Euler216.main, "5437849", "Primality of 2n^2-1"),
  (217, 0.3,  1, Euler217.main, "6273134", "Balanced Numbers"),
  (218, 0.0,  1, Euler218.main, "0", "Perfect right-angled triangles"),
  (219, 0.0,  1, Euler219.main, "64564225042", "Skew-cost coding"),
  (220, 0.0,  1, Euler220.main, "139776,963904", "Heighway Dragon"),
  (221,  10, 47, Euler221.main, Euler221.answer, "Alexandrian Integers"),
  (222, 0.0,  1, Euler222.main, "1590933", "Sphere Packing"),
  (223, 9.4,  1, Euler223.main, Euler223.answer, "Almost right-angled triangles I"),
  (224, 0.7,  1, Euler224.main, Euler224.answer, "Almost right-angled triangles II"),
  (225, 0.2,  1, Euler225.main, Euler225.answer, "Tribonacci non-divisors"),
  (226, 0.0,  1, Euler226.main, "0.11316017", "A Scoop of Blancmange"),
  (227, 0.2,  3, Euler227.main, Euler227.answer, "The Chase"),
  (228, 0.0,  1, Euler228.main, "86226", "Minkowski Sums"),
  (229,  15, 15, Euler229.main, Euler229.answer, "Four Representations using Squares"),
  (230, 0.0,  1, Euler230.main, Euler230.answer, "Fibonacci Words"),
  (231, 1.3,  3, Euler231.main, Euler231.answer, "Prime Factorisation of Binomial Coefficients"),
  (232, 0.1,  2, Euler232.main, Euler232.answer, "The Race"),
  (233,  13, 51, Euler233.main, Euler233.answer, "Lattice Points on a Circle"),
  (234, 3.0,  7, Euler234.main, Euler234.answer, "Semidivisible Numbers"),
  (235, 0.0,  1, Euler235.main, Euler235.answer, "An Arithmetic Geometric Sequence"),
  (236,  37,  2, Euler236.main, Euler236.answer, "Luxury Hampers"),
  (237, 0.1,  1, Euler237.main, Euler237.answer, "Tours on a 4xn Playing Board"),
  -- (238, 999, Euler238.main, Euler238.answer, "Infinite String Tour"),
  (239, 0.0,  1, Euler239.main, Euler239.answer, "Twenty-two Foolish Primes"),
  (240, 0.3, 10, Euler240.main, Euler240.answer, "Top Dice"),
  -- 241  "Perfection Quotients"
  (242, 0.0,  1, Euler242.main, Euler242.answer, "Odd Triplets"),
  -- 243  (found by trial and error) "Resilience"
  (244, 0.6,  8, Euler244.main, Euler244.answer, "Sliders"),
  (245, 316,103, Euler245.main, Euler245.answer, "Coresilience"),
  (246, 0.2,  1, Euler246.main, Euler246.answer, "Tangents to an Ellipse"),
  (247, 1.5, 45, Euler247.main, Euler247.answer, "Squares Under a Hyperbola"),
  (248, 3.4,  3, Euler248.main, Euler248.answer, "Totient Equals 13!"),
  (249, 104, 73, Euler249.main, Euler249.answer, "Prime Subset Sums"),
  (250,  11,  2, Euler250.main, Euler250.answer, "250250"),
  (251, 547,565, Euler251.main, Euler251.answer, "Cardano Triplets"),
  (252,  35,  2, Euler252.main, Euler252.answer, "Convex Holes"),
  (253, 0.0,  1, Euler253.main, Euler253.answer, "Tidying Up"),
  (254, 8.9,  2, Euler254.main, Euler254.answer, "Sum of Digit Factorials"),
  (255, 391,  1, Euler255.main, Euler255.answer, "Rounded Square Roots"),
  (256, 1.2,  2, Euler256.main, Euler256.answer, "Tatami-Free Rooms"),
  (257,  18,  2, Euler257.main, Euler257.answer, "Angular Bisectors"),
  (258,  62,  2, Euler258.main, Euler258.answer, "A Lagged Fibonacci Sequence"),
  (259,  45,221, Euler259.main, Euler259.answer, "Reachable Numbers"),
  (260,  43, 13, Euler260.main, Euler260.answer, "Stone Game"),
  (261, 0.2,  7, Euler261.main, Euler261.answer, "Pivotal Square Sums"),
  -- 262 "Mountain Range"
  (263,  34,  2, Euler263.main, Euler263.answer, "An engineer's dream come true"),
  -- 264 "Triangle Centres"
  (265, 0.1,  1, Euler265.main, Euler265.answer, "Binary Circles"),
  (266, 6.0, 40, Euler266.main, Euler266.answer, "Pseudo Square Root"),
  (267, 0.3, 14, Euler267.main, Euler267.answer, "Billionaire"),
  (268, 9.9,  1, Euler268.main, Euler268.answer, "Four divisors below 100"),
  (269, 1.0, 56, Euler269.main, Euler269.answer, "Polynomials with at least one integer root"),
  (270, 0.0,  1, Euler270.main, Euler270.answer, "Cutting Squares"),
  (271, 0.0,  1, Euler271.main, Euler271.answer, "Modular Cubes, Part 1"),
  (272, 330, 68, Euler272.main, Euler272.answer, "Modular Cubes, Part 2"),
  (273,  19,606, Euler273.main, Euler273.answer, "Sum of Squares"),
  (274, 7.0, 24, Euler274.main, Euler274.answer, "Divisibility Multipliers"),
  -- 275 "Balanced Sculptures"
  (276,  42,  1, Euler276.main, Euler276.answer, "Primitive Triangles"),
  (277, 0.0,  1, Euler277.main, Euler277.answer, "A Modified Collatz Sequence"),
  (278, 1.2,  2, Euler278.main, Euler278.answer, "Linear Combinations of Semiprimes"),
  -- 279 "Triangles with integral sides and an integral angle"
  -- 280 "Ant and seeds"
  -- 281 "Pizza Toppings"
  -- 282 "The Ackermann function"
  -- 283 "Integer sided triangles for which the area/perimeter ratio is integral"
  (284, 505,  3, Euler284.main, Euler284.answer, "Steady Squares"),
  (285, 0.2, 11, Euler285.main, Euler285.answer, "Pythagorean odds"),
  -- 286 "Scoring probabilities" (solved)
  (287, 122,  2, Euler287.main, Euler287.answer, "Quadtree encoding"),
  (288,  20,  2, Euler288.main, Euler288.answer, "An enormous factorial"),
  -- 289 "Eulerian Cycles"
  (290, 4.1,151, Euler290.main, Euler290.answer, "Digital Signature"),
  (291,2761,  2, Euler291.main, Euler291.answer, "Panaitopol Primes"),
  -- 292 "Pythagorean Polygons"
  (293, 0.2,  2, Euler293.main, Euler293.answer, "Pseudo-Fortunate Numbers"),
  (294, 0.2, 14, Euler294.main, Euler294.answer, "Sum of digits - experience #23"),
  -- 295 "Lenticular Holes"
  -- 296 "Angular Bisector and Tangent"
  (297, 0.0,  1, Euler297.main, Euler297.answer, "Zeckendorf Representation")
  -- 298 "Selective Amnesia"
  -- 299 "Three similar triangles"
  -- 300 "Protein folding"
  ]
