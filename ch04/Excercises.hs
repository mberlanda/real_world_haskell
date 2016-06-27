--file: ch04/Excercises.hs
module Excercises where

import Testing
import Data.Char  (digitToInt, isDigit)

-- Half Chapter
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
onlyFirstWord s = unlines.fstw.lines$s


-- ex.04 Write a program that transposes the text in a file. For instance, it should convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n"

myTranspose :: [[a]] -> [[a]]
myTranspose xs
    | null xs = []
    | any null xs = []
    | otherwise = (map head xs):myTranspose (map tail xs)

transposeText :: String -> String
transposeText [] = []
transposeText s  = unlines.myTranspose.lines$s 

-- End Chapter
-- ex. 01 Use a fold to rewrite and improve upon the asInt 

asInt_fold :: String -> Int
asInt_fold ('-':xs) = (-1) * asInt_fold xs
asInt_fold  xs      = foldl (\acc x -> acc * 10 + digitToInt x) 0 xs

testAsInt_fold :: (String, Int) -> Bool
testAsInt_fold (xs, n) = asInt_fold xs == n

ex1Test :: [Test]
ex1Test = [ Test "asInt_fold test" testAsInt_fold 
            [("101", 101), ("1798", 1798), ("-31337", -31337)]
          ]

-- ex. 02 The asInt_fold function uses error, so its callers cannot handle errors
type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either "" = Left "Empty List"
asInt_either xs
  | (any (not . isDigit) cls) = Left "Not an integer"
  | otherwise = Right (asInt_fold xs)
  where cls = dropWhile (== '-') xs

testAsInt_either :: (String, Either ErrorMessage Int) -> Bool
testAsInt_either (xs, r) = asInt_either xs == r

ex2Test :: [Test]
ex2Test = [ Test "asInt_fold test" testAsInt_either 
            [("", Left "Empty List"), ("1798", Right 1798),
             ("-31337", Right (-31337)), ("12-55", Right 1255)]
          ]

-- ex. 03 Write your own definition of concat using foldr
concat' :: [[a]] -> [a]
concat' = foldr (++) []

testConcat' :: Eq a => ([[a]], [a]) -> Bool
testConcat' (xs, x) = concat' xs == x

ex3Test :: [Test]
ex3Test = [Test "concat' test" testConcat'
           [([[1..10], [15..20]], [1,2,3,4,5,6,7,8,9,10,15,16,17,18,19,20])]
          ]