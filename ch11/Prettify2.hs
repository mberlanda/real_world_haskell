-- file: ch11/Prettify2.hs
module Prettify2(
    Doc(..), empty, isEmpty, (<>)
) where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show, Eq)

-- Testing Document Construction
empty :: Doc
empty = Empty

isEmpty :: Doc -> Bool
isEmpty doc = doc == empty

(<>) :: Doc -> Doc -> Doc
d1 <> d2
  | (isEmpty $ d1) && (isEmpty $ d2) = empty
  | isEmpty d1                       = d2
  | isEmpty d2                       = d1
  | otherwise                        = Concat d1 d2

-- Using Lists as a Model
fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty
