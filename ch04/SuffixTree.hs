-- file: ch04/SuffixTree.hs

import Data.List (tails)

noAsPattern :: [a] -> [[a]]
noAsPattern (x:xs) = (x:xs) : noAsPattern xs
noAsPattern _ = []

-- as-patterns @ implementation
suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _ = []

-- Code reuse through composition implementation
suffixes2 :: [a] -> [[a]]
suffixes2 xs = init (tails xs)

-- applying a function, then applying another function to its result
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

-- compose implementation
suffixes3 :: [a] -> [[a]]
suffixes3 xs = compose init tails xs

suffixes4 :: [a] -> [[a]]
suffixes4 = compose init tails

suffixes5 :: [a] -> [[a]]
suffixe5 = init . tails