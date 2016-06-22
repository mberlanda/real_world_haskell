-- file: ch04/IntParse.hs
module IntParse

where

  import Data.Char (digitToInt)

  asInt' :: String -> Int
  asInt' = foldl(\acc c -> acc * 10 + digitToInt c) 0

  loop :: Int -> String -> Int
  loop acc [] = acc
  loop acc (x:xs) = let acc' = acc *10 + digitToInt x
                    in loop acc' xs

  asInt :: String -> Int
  asInt xs = loop 0 xs