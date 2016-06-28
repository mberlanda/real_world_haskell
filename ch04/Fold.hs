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
append xs ys = myFoldr (:) ys xs

-- Space leaks and strict evaluation
foldl_seq  :: (a -> b -> a) -> a -> [b] -> a
foldl_seq _    zero []     = zero
foldl_seq step zero (x:xs) =
    let new = step zero x
    in  new `seq` foldl_seq step new xs

--  when a seq expression is evaluated, it forces its first argument to be evaluated, then returns its second argument. It doesn't actually do anything with the first argument: seq exists solely as a way to force that value to be evaluated. 
-- e.g. foldl_seq (+) 3 []

-- Learning to use seq

-- incorrect: seq is hidden by the application of someFunc
-- since someFunc will be evaluated first, seq may occur too late
hiddenInside x y = someFunc (x `seq` y)
    where someFunc = undefined

-- incorrect: a variation of the above mistake
hiddenByLet x y z = let a = x `seq` someFunc y
                    in anotherFunc a z
                    where someFunc = undefined
                          anotherFunc = undefined

-- correct: seq will be evaluated first, forcing evaluation of x
onTheOutside x y = x `seq` someFunc y
    where someFunc = undefined

chained x y z = x `seq` y `seq` someFunc z
    where someFunc = undefined

-- A common mistake is to try to use seq with two unrelated expressions
badExpression step zero (x:xs) =
    seq (step zero x)
        (badExpression step (step zero x) xs)

-- seq stops as soon as it reaches a constructor
strictPair :: (a,b) -> (a,b)
strictPair (a,b) = a `seq` b `seq` (a,b)

strictList :: [a] -> [a]
strictList (x:xs) = x `seq` x : strictList xs
strictList []     = []