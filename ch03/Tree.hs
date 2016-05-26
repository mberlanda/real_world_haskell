-- file: ch03/Tree.hs
module Tree where

-- Recursive Type
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

-- ex. 9 write a function that determinates the height of a tree

treeHeight :: (Tree a) -> Int
treeHeight Empty = 0
treeHeight (Node _ Empty Empty) = 1
treeHeight (Node _ tl tr) = max lH rH
  where
    lH = 1 + treeHeight tl 
    rH = 1 + treeHeight tr