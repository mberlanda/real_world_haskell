-- file: ch04/Filter.hs
module Filter
where

  oddList :: [Int] -> [Int]
  oddList (x:xs) | odd x     = x : oddList xs
                 | otherwise = oddList xs
  oddList _                  = []