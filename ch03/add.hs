-- file: ch03/add.hs
-- Pattern Matching
myNot :: Bool -> Bool 
myNot True = False
myNot False = True

sumList :: (Num a) => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList(xs)

