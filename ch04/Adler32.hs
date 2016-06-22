-- file: ch04/Adler32.hs

module Adler32
where

  import Data.Char (ord)
  import Data.Bits (shiftL, (.&.), (.|.))

  base = 65521

  adler32 :: [Char] -> Int
  adler32 xs = helper 1 0 xs
    where helper a b (x:xs) = let a' = (a + (ord x .&. 0xff)) `mod` base
                                  b' = (a' + b) `mod` base
                              in helper a' b' xs
          helper a b _      = (b `shiftL` 16) .|. a

  adler32_try2 :: [Char] -> Int
  adler32_try2 xs = helper (1,0) xs
    where helper (a,b) (x:xs) =
              let a' = (a + (ord x .&. 0xff) ) `mod` base
                  b' = (a' + b ) `mod` base
              in helper (a', b') xs
          helper (a, b) _     = (shiftL b 16) .|. a