module Euler4 where

import qualified Euler301
import qualified Euler303
import qualified Euler306
import qualified Euler313
import qualified Euler314
import qualified Euler317
import qualified Euler321
import qualified Euler325
import qualified Euler327
import qualified Euler328

checks :: [(Int, Double, Int, IO String, String, String)]
checks = [
  (301, 0.0,  1, Euler301.main, Euler301.answer, "Nim"),
  (303, 9.9,  3, Euler303.main, Euler303.answer, "Multiples with small digits"),
  (306, 0.0,  1, Euler306.main, Euler306.answer, "Paper-strip Game"),
  (313, 0.3,  4, Euler313.main, Euler313.answer, "Sliding game"),
  (314, 184,324, Euler314.main, Euler314.answer, "The Mouse on the Moon"),
  (317, 0.0,  1, Euler317.main, Euler317.answer, "Firecracker"),
  (321, 0.0,  1, Euler321.main, Euler321.answer, "Swapping Counters"),
  (325, 0.0,  1, Euler325.main, Euler325.answer, "Stone Game II"),
  (327, 0.0,  1, Euler327.main, Euler327.answer, "Rooms of Doom"),
  (328, 999,999, Euler328.main, Euler328.answer, "Lowest-cost Search")
  ]
