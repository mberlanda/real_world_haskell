-- file: ch11/QC.hs

import Test.QuickCheck
import Prettify2
import Control.Monad( liftM, liftM2 )

instance Arbitrary Doc where
    arbitrary =
        oneof [ return Empty
              , liftM Char arbitrary
              , liftM Text arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union arbitrary arbitrary ]


-- Testing Document Construction

prop_empty_id x =
    empty <> x == x
  &&
    x <> empty == x

-- prop_char c   = char c   == Char c
-- prop_text s   = text s   == if null s then Empty else Text s
-- prop_line     = line     == Line
-- prop_double d = double d == text (show d)

-- Using Lists as a Model
{-
import Text.PrettyPrint(hcat)
prop_hcat xs = hcat xs == glue xs
  where
    glue [] = empty
    glue (d:ds) = d <> glue ds
-}

{-
import Data.List(intersperse)
prop_punctuate s xs = punctuate s xs == intersperse s xs
prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
  where combine []           = []
        combine [x]          = [x]
        combine (x:Empty:ys) = x : combine ys
        combine (Empty:y:ys) = y : combine ys
        combine (x:y:ys)     = x `Concat` y : combine ys
-}
