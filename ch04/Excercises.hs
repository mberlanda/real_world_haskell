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