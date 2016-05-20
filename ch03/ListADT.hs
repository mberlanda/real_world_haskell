-- file: ch03/ListADT.hs
-- Recursive Types
data List a = Cons a (List a)
            | Nil
             deriving (Show)

fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

-- exercise
toList :: List a -> [a]
toList Nil = []
toList (Cons x (xs)) = x: (toList xs) 