-- file: ch05/PrettyStub.hs
import SimpleJSON

data Doc = ToBeDefined
         deriving (Show)

string :: String -> Doc
string str = undefined

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined