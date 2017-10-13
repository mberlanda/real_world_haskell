-- file: ch11/Arbitrary.hs

import Test.QuickCheck
import Prettify2
import Control.Monad( liftM, liftM2 )

data Ternary = Yes
             | No
             | Unknown
             deriving(Eq, Show)

instance Arbitrary Ternary where
    arbitrary = do
        n <- choose (0, 2) :: Gen Int
        return $ case n of
                      0 -> Yes
                      1 -> No
                      _ -> Unknown


{- Doc first implementation

instance Arbitrary Doc where
    arbitrary = do
        n <- choose (1,6) :: Gen Int
        case n of
            1 -> return Empty
            2 -> do x <- arbitrary
                    return (Char x)
            3 -> do x <- arbitrary
                    return (Text x)
            4 -> return Line
            5 -> do x <- arbitrary
                    y <- arbitrary
                    return (Concat x y)
            6 -> do x <- arbitrary
                    y <- arbitrary
                    return (Union x y)
-}

instance Arbitrary Doc where
    arbitrary =
        oneof [ return Empty
              , liftM Char arbitrary
              , liftM Text arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union arbitrary arbitrary ]
