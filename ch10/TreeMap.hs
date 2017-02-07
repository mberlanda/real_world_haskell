-- file: ch10/TreeMap.hs
module TreeMap(Tree(..), TreeMap.Functor) where

  data Tree a = Node (Tree a) (Tree a)
              | Leaf a
                deriving (Show)

  treeLengths :: Foldable t => Tree (t a) -> Tree Int
  treeLengths (Leaf s) = Leaf (length s)
  treeLengths (Node l r) = Node (treeLengths l) (treeLengths r)

  treeMap :: (a -> b) -> Tree a -> Tree b
  treeMap f (Leaf a)   = Leaf (f a)
  treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)

  class Functor f where
    fmap :: (a -> b) -> f a -> f b

  instance TreeMap.Functor Tree where
    fmap = treeMap
  
  instance TreeMap.Functor [] where
    fmap = map

  instance TreeMap.Functor Maybe where
    fmap _ Nothing  = Nothing
    fmap f (Just x) = Just (f x)