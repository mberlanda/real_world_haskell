-- file: ch02/exercises.hs
myLast :: [a] -> a
myLast [] = error "last of empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

myLast' :: [a] -> a
myLast' [] = error "last of empty list"
myLast' (x:xs)
    | null xs     = x
    | otherwise   = myLast' xs

myLast'' :: [a] -> a
myLast'' [] = error "last of empty list"
myLast'' (x:xs) = if (length $ xs) == 0 then x else myLast'' $ xs

lastButOne :: [a] -> a
lastButOne [] = error "empty list"
lastButOne (x:xs)
    | (length $ (x:xs)) <= 1 = error "list too short"
    | (length $ xs) == 1     = x
    | otherwise              = lastButOne $ xs