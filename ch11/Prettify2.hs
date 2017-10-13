-- file: ch11/Prettify2.hs
module Prettify2(
    Doc(..)
) where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show, Eq)
