-- file: Map.hs

module Map
where

  import Data.Char (toUpper)

  upperCase :: String -> String
  upperCase [] = []
  upperCase (x:xs) = toUpper x : upperCase xs

  upperCase2 :: String -> String
  upperCase2 xs = map toUpper xs

  square :: [Double] -> [Double]
  square [] = []
  square (x:xs) = x*x : square xs

  square2 :: [Double] -> [Double]
  square2 xs = map squareOne xs
    where squareOne x = x*x

  myMap :: (a -> b) -> [a] -> [b]
  myMap f (x:xs) = f x : myMap f xs
  myMap _ _ = [] 