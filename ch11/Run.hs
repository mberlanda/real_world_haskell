-- file: ch11/Run.hs

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

prop_empty_id x = empty <> x == x && x <> empty == x

{-
options = TestOptions {
      no_of_tests = 200
    , length_of_tests = 1
    , debug_tests = False
-}

args = stdArgs {
    maxSuccess = 200
  , maxSize = 1
  , chatty = True
}


main = do
      quickCheckWith args prop_empty_id
