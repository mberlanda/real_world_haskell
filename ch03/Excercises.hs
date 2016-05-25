-- file: ch03/Excercises.hs

-- 1. write myLenght and test it with default lenght function
myLenght :: [a] -> Integer
myLenght [] = 0
myLenght (_:xs) = 1 + myLenght xs

-- 2. define a tyoe signature
data Functions = Lenght
               | Sum
               | Average
              deriving(Show, Eq)

-- 3. write a function to compute the mean
mean :: [Integer] -> Double
mean l
  | len(l) <= 0 = 0
  | otherwise    = (fromIntegral $ sum'(l)) / (fromIntegral $ len(l))
  where
    len [] = 0
    len (_:xs) = 1 + len xs
    sum' [] = 0
    sum' (x:xs) = x + sum' xs

-- 4. turn a list into a palindrome
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