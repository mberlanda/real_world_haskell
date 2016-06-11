-- file: ch04/SplitLines.hs
module SplitLines where

  isLineSeparator :: Char -> Bool
  isLineSeparator = (`elem`['\r', '\n'])


  splitLines :: String -> [String]
  splitLines [] = []
  splitLines cs =
    let (pre, suf) = break isLineSeparator cs
    in pre : case suf of
      ('\r':'\n':rest) -> splitLines rest
      ('\n':rest)      -> splitLines rest
      ('\r':rest)      -> splitLines rest
      _                -> []

  fixLines :: String -> String
  fixLines input = unlines' (splitLines input)
    where unlines' (l:ls) = l ++ '\n' : unlines' ls
          unlines' [] = []