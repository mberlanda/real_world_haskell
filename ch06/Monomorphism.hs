-- file: ch06/Monomorphism.hs
{-
myShow = show
ch06/Monomorphism.hs:2:10:
    No instance for (Show a0) arising from a use of `show'
    The type variable `a0' is ambiguous
    Possible cause: the monomorphism restriction applied to the following:
      myShow :: a0 -> String (bound at ch06/Monomorphism.hs:2:1)
-}

myShow2 value = show value

myShow3 :: (Show a) => a -> String
myShow3 = show