--file: ch04/Excercises.hs

-- ex. 01
safeHead :: [a] -> Maybe a
safeHead  [] = Nothing
safeHead l@(x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail [x] = Just []
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit [x] = Just []
safeInit (x:xs) = Just (init' x xs)
  where init' _ []     = []
        init' y (z:zs) = y : init' z zs

-- ex.02 Write a function splitWith that acts similarly to words, but takes a predicate and a list of any type, and splits its input list on every element for which the predicate returns False.

-- words implementation
myIsSpace :: Char -> Bool
myIsSpace = (`elem` [' '])

myWords :: String -> [String]
myWords s = case dropWhile myIsSpace s of
                "" -> []
                s' -> w: myWords s''
                      where (w, s'') = break myIsSpace s'

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p s = case dropWhile p s of
                  [] -> []
                  s' -> w : splitWith p s''
                        where (w, s'') = break p s'

-- ex.03  write a program that prints the first word of each line of its input

fstw :: [String] -> [String]
fstw [] = []
fstw (x:xs) = fst x : fstw xs
              where fst [] = []
                    fst xs = head(words(xs))

onlyFirstWord :: String -> String
onlyFirstWord [] = []
onlyFirstWord s = unlines(fstw(lines$s))



