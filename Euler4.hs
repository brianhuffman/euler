module Euler4 where

import qualified Euler301
import qualified Euler303
import qualified Euler304
import qualified Euler306
import qualified Euler307
import qualified Euler310
import qualified Euler313
import qualified Euler314
import qualified Euler315
import qualified Euler317
import qualified Euler318
import qualified Euler321
import qualified Euler322
import qualified Euler325
import qualified Euler326
import qualified Euler327
import qualified Euler328
import qualified Euler329

checks :: [(Int, Double, Int, IO String, String, String)]
checks = [
  (301, 0.0,  1, Euler301.main, Euler301.answer, "Nim"),
  (303, 9.9,  3, Euler303.main, Euler303.answer, "Multiples with small digits"),
  (304,  43,  2, Euler304.main, Euler304.answer, "Primonacci"),
  (306, 0.0,  1, Euler306.main, Euler306.answer, "Paper-strip Game"),
  (307, 2.4,  4, Euler307.main, Euler307.answer, "Chip Defects"),
  (310, 4.6,  6, Euler310.main, Euler310.answer, "Nim Square"),
  (313, 0.3,  4, Euler313.main, Euler313.answer, "Sliding game"),
  (314, 184,324, Euler314.main, Euler314.answer, "The Mouse on the Moon"),
  (315,  11, 56, Euler315.main, Euler315.answer, "Digital root clocks"),
  (317, 0.0,  1, Euler317.main, Euler317.answer, "Firecracker"),
  (318, 0.0,  1, Euler318.main, Euler318.answer, "2011 nines"),
  (321, 0.0,  1, Euler321.main, Euler321.answer, "Swapping Counters"),
  (322, 0.0,  0, Euler322.main, Euler322.answer, "Binomial coefficients divisible by 10"),
  (325, 0.0,  1, Euler325.main, Euler325.answer, "Stone Game II"),
  (326,  25,287, Euler326.main, Euler326.answer, "Modulo Summations"),
  (327, 0.0,  1, Euler327.main, Euler327.answer, "Rooms of Doom"),
  (328, 999,999, Euler328.main, Euler328.answer, "Lowest-cost Search"),
  (329, 0.0,  2, Euler329.main, Euler329.answer, "Prime Frog")
  ]
