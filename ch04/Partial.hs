-- file: ch04/Partial.hs
module Partial where
import Data.List (isInfixOf)

isInAny :: (Foldable t, Eq a)  => [a] -> t [a] -> Bool
isInAny needle haystack = any inSequence haystack
    where inSequence s = needle `isInfixOf` s

-- Anonymous (lambda) function implementation
isInAny2 :: (Foldable t, Eq a)  => [a] -> t [a] -> Bool
isInAny2 needle haystack = any (\s  -> needle `isInfixOf` s) haystack

isInAny2' :: (Foldable t, Eq a)  => [a] -> t [a] -> Bool
isInAny2' needle = any (\s -> isInfixOf needle s)