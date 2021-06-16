-- Make scanl into a good list producer / consumer.

{- OPTIONS -frewrite-rules -}

module Scanl ( ) where
import qualified GHC.Base

{-# NOINLINE [0] scanlFB #-}
scanlFB :: (a -> b -> b) -> (a -> c -> a) -> c -> (a -> b) -> (a -> b)
scanlFB c f x xs z = z `c` xs (f z x)

{-# NOINLINE [0] scanlNil #-}
scanlNil :: (a -> b -> b) -> b -> a -> b
scanlNil c n z = z `c` n

{-# RULES
"scanl" [~1]     forall f z xs. scanl f z xs = GHC.Base.build (\c n -> foldr (scanlFB c f) (scanlNil c n) xs z)
"scanlList" [1]  forall f z xs. foldr (scanlFB (:) f) (scanlNil (:) []) xs z = scanl f z xs
 #-}
