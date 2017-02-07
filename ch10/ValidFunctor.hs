--file: ch10/ValidFunctor.hs
import TreeMap

data Eq a => Bar a = Bar a

instance TreeMap.Functor Bar where
    fmap f (Bar a) = Bar (f a)

-- this does not compile as expected
