-- file: ch04/dlts.hs
import Data.List (isPrefixOf)

-- Our goal is to extract names such as DLT_EN10MB and DLT_AX25
dlts :: String -> [String]
dlts = foldr step [] . lines
  where step l ds
          | "#define DLT_" `isPrefixOf` l = secondWord l : ds
          | otherwise                     = ds
        secondWord = head . tail . words