-- Make takeWhile into a good list producer / consumer.

{- OPTIONS -frewrite-rules -}

module TakeWhile ( ) where
import qualified GHC.Base

{-# NOINLINE [0] takeWhileFB #-}
takeWhileFB c n p x r | p x       = x `c` r
                      | otherwise = n

{-# RULES
"takeWhile" [~1]     forall p xs. takeWhile p xs = GHC.Base.build (\c n -> foldr (takeWhileFB c n p) n xs)
"takeWhileList" [1]  forall p. foldr (takeWhileFB (:) [] p) [] = takeWhile p
 #-}
