-- file: ch05/Prettify.hs
module Prettify (
  Doc, (<>), char, double, fsep,
  hcat, punctuate, text,compact,
  pretty, fill, nest) where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

compact :: Doc -> String
compact x = transform [x]
  where transform [] = ""
        transform (d:ds) = 
          case d of
            Empty        -> transform ds
            Char c       -> c : transform ds
            Text s       -> s ++ transform ds
            Line         -> '\n' : transform ds
            a `Concat` b -> transform (a:b:ds)
            _ `Union` b  -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
  where best col (d:ds) = 
          case d of
            Empty        -> best col ds
            Char c       -> c : best (col+1) ds
            Text s       -> s ++ best (col + length s) ds
            Line         -> '\n' : best 0 ds
            a `Concat` b -> best col (a:b:ds)
            a `Union` b  -> nicest col (best col (a:ds))
                                       (best col (b:ds))
        best _ _ = ""

        nicest col a b | (width - least) `fits` a = a
                       | otherwise                = b
                       where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w-1) `fits` cs

-- http://stackoverflow.com/a/32346509/5687152
-- ex. 01 fill function should add spaces to a document until it is the given number of columns wide. If it is already wider than this value, it should add no spaces
fill :: Int ->  Doc -> Doc
fill width x = hcat (init (scanLines 0 [x <> Line]))
  where
    scanLines _ []         = []
    scanLines col (d:ds) =
      case d of
        Empty        -> scanLines col ds
        Char c       -> Char c : scanLines (col + 1) ds
        Text s       -> Text s : scanLines (col + length s) ds
        Line         -> Text (padLine (width - col)) : Line : scanLines 0 ds
        a `Concat` b -> scanLines col (a:b:ds)
        _ `Union` b  -> scanLines col (b:ds)
    padLine w = replicate w ' '

-- ex. 02 nest function for indentation
nest :: Int -> Doc -> Doc
nest indentation x = indent 0 [x]
  where
    indent _ []             = Empty
    indent nestLevel (d:ds) =
      case d of
        Empty        -> indent nestLevel ds
        Char '{'     -> padLine nestLevel <> Char '{' <> indent (nestLevel + 1) (Line:ds)
        Char '}'     -> padLine (nestLevel - 1) <> Char '}' <> indent (nestLevel - 1) ds
        Char '['     -> padLine nestLevel <> Char '[' <> indent (nestLevel + 1) (Line:ds)
        Char ']'     -> padLine (nestLevel - 1) <> Char ']' <> indent (nestLevel - 1) ds
        Char c       -> Char c <> indent nestLevel ds
        Text s       -> Text s <> indent nestLevel ds
        Line         -> padLine nestLevel <> indent nestLevel ds
        a `Concat` b -> indent nestLevel (a:b:ds)
        a `Union` b  -> indent nestLevel (a:ds) `Union` indent nestLevel (b:ds)
    padLine nl = Line <> Text (replicate (nl * indentation) ' ')