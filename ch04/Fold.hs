-- file: ch04/Fold.hs

module Fold where

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl step zero (x:xs) = myFoldl step (step zero x) xs
myFoldl _    zero []     = zero

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr step zero (x:xs) = step x (myFoldr step zero xs)
myFoldr _    zero []     = zero

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

-- primitive recursive

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p xs = myFoldr step [] xs
  where step x ys | p x       = x : ys
                  | otherwise = ys

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = myFoldr step [] xs
  where step x ys = f x : ys

myFoldl' :: (a -> b -> a) -> a -> [b] -> a
myFoldl' f z xs = myFoldr step id xs z
  where step x g a = g (f a x)

identity :: [a] -> [a]
identity xs = myFoldr (:) [] xs

append :: [a] -> [a] -> [a]
identity xs ys = myFoldr (:) ys xs
