-- file: ch03/Excercises.hs

import Data.List

-- 1. write myLenght and test it with default lenght function
len :: [a] -> Integer
len [] = 0
len (_:xs) = 1 + len xs

-- 2. define a type signature
data Len a = Len a
           deriving(Show, Eq, Ord)

-- 3. write a function to compute the mean
mean :: [Integer] -> Double
mean l
  | len(l) <= 0 = 0
  | otherwise    = (fromIntegral $ sum'(l)) / (fromIntegral $ len(l))
  where
    sum' [] = 0
    sum' (x:xs) = x + sum' xs

-- 4. turn a list into a palindrome
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev(xs) ++ [x]


pal :: [a] -> [a]
pal [] = []
pal xs = xs ++ (rev $ xs)
     where rev [] = []
           rev (x:xs) = rev(xs) ++ [x]

-- 5. write a function that determines whether its input list is a palindrome
isPal :: Eq a => [a] -> Bool
isPal [] = True
isPal [x] = True
isPal (x:xs)
  | x == (lst xs) = isPal.init $ xs
  | otherwise     = False 
  where lst [x]    =  x
        lst (_:xs) =  lst xs

isPal' :: Eq a => [a] -> Bool
isPal' xs = xs == (rev xs)

-- 6. write a function that sorts a list of lists based on the lenght of each sublist
cmp :: (Ord a) => (b -> a) -> b -> b -> Ordering
cmp p x y = compare (p x) (p y)

sortByLen :: [[a]] -> [[a]]
sortByLen xs = sortBy (cmp len) xs

-- 7. create a function that joins a list with a separator
-- file: ch03/Intersperse.hs