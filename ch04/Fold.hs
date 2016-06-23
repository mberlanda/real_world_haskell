-- file: ch04/Fold.hs

module Fold where

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl step zero (x:xs) = myFoldl step (step zero x) xs
myFoldl _    zero []     = zero